package actors

import actors.DrtStaticParameters.expireAfterMillis
import actors.PartitionedPortStateActor.GetFlights
import actors.daily.PassengersActor
import actors.queues.FlightsRouterActor
import actors.queues.QueueLikeActor.UpdatedMillis
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Cancellable, Props, Scheduler}
import akka.pattern.ask
import akka.stream.scaladsl.{Sink, Source, SourceQueueWithComplete}
import akka.stream.{Materializer, OverflowStrategy, UniqueKillSwitch}
import akka.util.Timeout
import controllers.{Deskstats, PaxFlow, UserRoleProviderLike}
import drt.chroma.chromafetcher.ChromaFetcher.{ChromaForecastFlight, ChromaLiveFlight}
import drt.chroma.chromafetcher.{ChromaFetcher, ChromaFlightMarshallers}
import drt.chroma.{ChromaFeedType, ChromaLive}
import drt.http.ProdSendAndReceive
import drt.server.feeds.acl.AclFeed
import drt.server.feeds.bhx.{BHXClient, BHXFeed}
import drt.server.feeds.chroma.{ChromaForecastFeed, ChromaLiveFeed}
import drt.server.feeds.cirium.CiriumFeed
import drt.server.feeds.common.{ArrivalFeed, HttpClient}
import drt.server.feeds.gla.{GlaFeed, ProdGlaFeedRequester}
import drt.server.feeds.lcy.{LCYClient, LCYFeed}
import drt.server.feeds.legacy.bhx.{BHXForecastFeedLegacy, BHXLiveFeedLegacy}
import drt.server.feeds.lgw.{LGWAzureClient, LGWFeed}
import drt.server.feeds.lhr.LHRFlightFeed
import drt.server.feeds.lhr.sftp.LhrSftpLiveContentProvider
import drt.server.feeds.ltn.{LtnFeedRequester, LtnLiveFeed}
import drt.server.feeds.mag.{MagFeed, ProdFeedRequester}
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.FlightsApi.{Flights, FlightsWithSplits}
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.Arrival
import manifests.ManifestLookupLike
import manifests.queues.SplitsCalculator
import org.joda.time.DateTimeZone
import play.api.Configuration
import queueus.{AdjustmentsNoop, ChildEGateAdjustments, PaxTypeQueueAllocation, QueueAdjustments}
import server.feeds.{ArrivalsFeedResponse, ArrivalsFeedSuccess, ManifestsFeedResponse}
import services.PcpArrival.{GateOrStandWalkTime, gateOrStandWalkTimeCalculator, walkTimeMillisProviderFromCsv}
import services._
import services.arrivals.{ArrivalsAdjustments, ArrivalsAdjustmentsLike}
import services.crunch.CrunchSystem.paxTypeQueueAllocator
import services.crunch.desklimits.{PortDeskLimits, TerminalDeskLimitsLike}
import services.crunch.deskrecs._
import services.crunch.{CrunchProps, CrunchSystem}
import services.graphstages.Crunch

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps

trait DrtSystemInterface extends UserRoleProviderLike {
  implicit val materializer: Materializer
  implicit val ec: ExecutionContext
  implicit val system: ActorSystem

  val now: () => SDateLike = () => SDate.now()
  val purgeOldLiveSnapshots = false
  val purgeOldForecastSnapshots = true

  val manifestLookupService: ManifestLookupLike

  val config: Configuration
  val airportConfig: AirportConfig
  val params: DrtConfigParameters = DrtConfigParameters(config)
  val journalType: StreamingJournalLike = StreamingJournal.forConfig(config)

  val gateWalkTimesProvider: GateOrStandWalkTime = walkTimeMillisProviderFromCsv(params.gateWalkTimesFilePath)
  val standWalkTimesProvider: GateOrStandWalkTime = walkTimeMillisProviderFromCsv(params.standWalkTimesFilePath)

  val alertsActor: ActorRef = system.actorOf(Props(new AlertsActor(now)), name = "alerts-actor")
  val liveBaseArrivalsActor: ActorRef = system.actorOf(Props(new LiveBaseArrivalsActor(params.snapshotMegaBytesLiveArrivals, now, expireAfterMillis)), name = "live-base-arrivals-actor")
  val arrivalsImportActor: ActorRef = system.actorOf(Props(new ArrivalsImportActor()), name = "arrivals-import-actor")
  val crunchQueueActor: ActorRef
  val deploymentQueueActor: ActorRef

  val minuteLookups: MinuteLookupsLike

  val portStateActor: ActorRef
  val shiftsActor: ActorRef
  val fixedPointsActor: ActorRef
  val staffMovementsActor: ActorRef
  val baseArrivalsActor: ActorRef
  val forecastArrivalsActor: ActorRef
  val liveArrivalsActor: ActorRef
  val manifestsRouterActor: ActorRef

  val flightsActor: ActorRef
  val queuesActor: ActorRef
  val staffActor: ActorRef
  val queueUpdates: ActorRef
  val staffUpdates: ActorRef
  val flightUpdates: ActorRef

  lazy private val feedActors: Map[FeedSource, ActorRef] = Map(
    LiveFeedSource -> liveArrivalsActor,
    LiveBaseFeedSource -> liveBaseArrivalsActor,
    ForecastFeedSource -> forecastArrivalsActor,
    AclFeedSource -> baseArrivalsActor,
    ApiFeedSource -> manifestsRouterActor
  )

  lazy val feedActorsForPort: Map[FeedSource, ActorRef] = feedActors.filter {
    case (feedSource: FeedSource, _) => isValidFeedSource(feedSource)
  }

  val aclFeed: AclFeed = AclFeed(params.ftpServer, params.username, params.path, airportConfig.feedPortCode, AclFeed.aclToPortMapping(airportConfig.portCode), params.aclMinFileSizeInBytes)

  val maxDaysToConsider: Int = 14
  val passengersActorProvider: () => ActorRef = () => system.actorOf(Props(new PassengersActor(maxDaysToConsider, aclPaxAdjustmentDays, now)), name = "passengers-actor")

  val aggregatedArrivalsActor: ActorRef

  val aclPaxAdjustmentDays: Int = config.get[Int]("acl.adjustment.number-of-days-in-average")

  val optimiser: TryCrunch = if (config.get[Boolean]("feature-flags.use-legacy-optimiser")) Optimiser.crunch else OptimiserWithFlexibleProcessors.crunch

  val portDeskRecs: PortDesksAndWaitsProviderLike = PortDesksAndWaitsProvider(airportConfig, optimiser)

  val deskLimitsProviders: Map[Terminal, TerminalDeskLimitsLike] = if (config.get[Boolean]("crunch.flex-desks"))
    PortDeskLimits.flexed(airportConfig)
  else
    PortDeskLimits.fixed(airportConfig)

  val paxTypeQueueAllocation: PaxTypeQueueAllocation = paxTypeQueueAllocator(airportConfig)

  val splitAdjustments: QueueAdjustments = if (params.adjustEGateUseByUnder12s)
    ChildEGateAdjustments(airportConfig.assumedAdultsPerChild)
  else
    AdjustmentsNoop

  def run(): Unit

  def walkTimeProvider(flight: Arrival): MillisSinceEpoch =
    gateOrStandWalkTimeCalculator(gateWalkTimesProvider, standWalkTimesProvider, airportConfig.defaultWalkTimeMillis.getOrElse(flight.Terminal, 300000L))(flight)

  def pcpArrivalTimeCalculator: Arrival => MilliDate =
    PaxFlow.pcpArrivalTimeForFlight(airportConfig.timeToChoxMillis, airportConfig.firstPaxOffMillis)(walkTimeProvider)

  def isValidFeedSource(fs: FeedSource): Boolean = airportConfig.feedSources.contains(fs)

  def startCrunchSystem(initialPortState: Option[PortState],
                        initialForecastBaseArrivals: Option[SortedMap[UniqueArrival, Arrival]],
                        initialForecastArrivals: Option[SortedMap[UniqueArrival, Arrival]],
                        initialLiveBaseArrivals: Option[SortedMap[UniqueArrival, Arrival]],
                        initialLiveArrivals: Option[SortedMap[UniqueArrival, Arrival]],
                        refreshArrivalsOnStart: Boolean,
                        refreshManifestsOnStart: Boolean,
                        startDeskRecs: () => (UniqueKillSwitch, UniqueKillSwitch)): CrunchSystem[Cancellable] = {

    val voyageManifestsLiveSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]] = Source.queue[ManifestsFeedResponse](1, OverflowStrategy.backpressure)

    val arrivalAdjustments: ArrivalsAdjustmentsLike = ArrivalsAdjustments.adjustmentsForPort(
      airportConfig.portCode,
      params.maybeEdiTerminalMapCsvUrl
    )

    val simulator: TrySimulator = if (config.get[Boolean]("feature-flags.use-legacy-optimiser")) Optimiser.runSimulationOfWork else OptimiserWithFlexibleProcessors.runSimulationOfWork

    val crunchInputs = CrunchSystem(CrunchProps(
      airportConfig = airportConfig,
      pcpArrival = pcpArrivalTimeCalculator,
      portStateActor = portStateActor,
      flightsActor = flightsActor,
      maxDaysToCrunch = params.forecastMaxDays,
      expireAfterMillis = DrtStaticParameters.expireAfterMillis,
      actors = Map(
        "shifts" -> shiftsActor,
        "fixed-points" -> fixedPointsActor,
        "staff-movements" -> staffMovementsActor,
        "forecast-base-arrivals" -> baseArrivalsActor,
        "forecast-arrivals" -> forecastArrivalsActor,
        "live-base-arrivals" -> liveBaseArrivalsActor,
        "live-arrivals" -> liveArrivalsActor,
        "aggregated-arrivals" -> aggregatedArrivalsActor,
        "deployment-request" -> deploymentQueueActor
      ),
      useNationalityBasedProcessingTimes = params.useNationalityBasedProcessingTimes,
      manifestsLiveSource = voyageManifestsLiveSource,
      voyageManifestsActor = manifestsRouterActor,
      simulator = simulator,
      initialPortState = initialPortState,
      initialForecastBaseArrivals = initialForecastBaseArrivals.getOrElse(SortedMap()),
      initialForecastArrivals = initialForecastArrivals.getOrElse(SortedMap()),
      initialLiveBaseArrivals = initialLiveBaseArrivals.getOrElse(SortedMap()),
      initialLiveArrivals = initialLiveArrivals.getOrElse(SortedMap()),
      arrivalsForecastBaseSource = baseArrivalsSource(),
      arrivalsForecastSource = forecastArrivalsSource(airportConfig.feedPortCode),
      arrivalsLiveBaseSource = liveBaseArrivalsSource(airportConfig.feedPortCode),
      arrivalsLiveSource = liveArrivalsSource(airportConfig.feedPortCode),
      passengersActorProvider = passengersActorProvider,
      initialShifts = initialState[ShiftAssignments](shiftsActor).getOrElse(ShiftAssignments(Seq())),
      initialFixedPoints = initialState[FixedPointAssignments](fixedPointsActor).getOrElse(FixedPointAssignments(Seq())),
      initialStaffMovements = initialState[StaffMovements](staffMovementsActor).map(_.movements).getOrElse(Seq[StaffMovement]()),
      refreshArrivalsOnStart = refreshArrivalsOnStart,
      refreshManifestsOnStart = refreshManifestsOnStart,
      adjustEGateUseByUnder12s = params.adjustEGateUseByUnder12s,
      optimiser = optimiser,
      aclPaxAdjustmentDays = aclPaxAdjustmentDays,
      startDeskRecs = startDeskRecs,
      arrivalsAdjustments = arrivalAdjustments
    ))
    crunchInputs
  }

  def arrivalsNoOp: Source[ArrivalsFeedResponse, Cancellable] = Source.tick[ArrivalsFeedResponse](100 days, 100 days, ArrivalsFeedSuccess(Flights(Seq()), SDate.now()))

  def baseArrivalsSource(): Source[ArrivalsFeedResponse, Cancellable] = if (isTestEnvironment)
    arrivalsNoOp
  else
    Source.tick(1 second, 10 minutes, NotUsed).map(_ => {
      system.log.info(s"Requesting ACL feed")
      aclFeed.requestArrivals
    })

  private def isTestEnvironment: Boolean = config.getOptional[String]("env").getOrElse("live") == "test"

  val startDeskRecs: () => (UniqueKillSwitch, UniqueKillSwitch) = () => {
    val staffToDeskLimits = PortDeskLimits.flexedByAvailableStaff(airportConfig) _

    implicit val timeout: Timeout = new Timeout(1 second)

    val splitsCalculator = SplitsCalculator(paxTypeQueueAllocation, airportConfig.terminalPaxSplits, splitAdjustments)

    val deskRecsProducer = DynamicRunnableDeskRecs.crunchRequestsToQueueMinutes(
      arrivalsProvider = OptimisationProviders.arrivalsProvider(portStateActor),
      liveManifestsProvider = OptimisationProviders.liveManifestsProvider(manifestsRouterActor),
      historicManifestsProvider = OptimisationProviders.historicManifestsProvider(airportConfig.portCode, manifestLookupService),
      splitsCalculator = splitsCalculator,
      splitsSink = portStateActor,
      flightsToLoads = portDeskRecs.flightsToLoads,
      loadsToQueueMinutes = portDeskRecs.loadsToDesks,
      maxDesksProviders = deskLimitsProviders)

    val (crunchRequestQueue, deskRecsKillSwitch) = RunnableOptimisation.createGraph(portStateActor, deskRecsProducer).run()

    val deploymentsProducer = DynamicRunnableDeployments.crunchRequestsToDeployments(
      OptimisationProviders.loadsProvider(minuteLookups.queueMinutesActor),
      OptimisationProviders.staffMinutesProvider(minuteLookups.staffMinutesActor, airportConfig.terminals),
      staffToDeskLimits,
      portDeskRecs.loadsToSimulations
    )

    val (deploymentRequestQueue, deploymentsKillSwitch) = RunnableOptimisation.createGraph(portStateActor, deploymentsProducer).run()

    crunchQueueActor ! SetCrunchRequestQueue(crunchRequestQueue)
    deploymentQueueActor ! SetCrunchRequestQueue(deploymentRequestQueue)

    if (params.recrunchOnStart) queueDaysToReCrunch(crunchQueueActor)

    (deskRecsKillSwitch, deploymentsKillSwitch)
  }

  def queueDaysToReCrunch(crunchQueueActor: ActorRef): Unit = {
    val today = now()
    val millisToCrunchStart = Crunch.crunchStartWithOffset(portDeskRecs.crunchOffsetMinutes) _
    val daysToReCrunch = (0 until params.forecastMaxDays).map(d => {
      millisToCrunchStart(today.addDays(d)).millisSinceEpoch
    })
    crunchQueueActor ! UpdatedMillis(daysToReCrunch)
  }

  def startScheduledFeedImports(crunchInputs: CrunchSystem[Cancellable]): Unit = {
    if (airportConfig.feedPortCode == PortCode("LHR")) params.maybeBlackJackUrl.map(csvUrl => {
      val requestIntervalMillis = 5 * MilliTimes.oneMinuteMillis
      Deskstats.startBlackjack(csvUrl, crunchInputs.actualDeskStats, requestIntervalMillis milliseconds, () => SDate.now().addDays(-1))
    })
  }

  def subscribeStaffingActors(crunchInputs: CrunchSystem[Cancellable]): Unit = {
    shiftsActor ! AddShiftSubscribers(List(crunchInputs.shifts))
    fixedPointsActor ! AddFixedPointSubscribers(List(crunchInputs.fixedPoints))
    staffMovementsActor ! AddStaffMovementsSubscribers(List(crunchInputs.staffMovements))
  }

  def liveBaseArrivalsSource(portCode: PortCode): Source[ArrivalsFeedResponse, Cancellable] = {
    if (config.get[Boolean]("feature-flags.use-cirium-feed")) {
      log.info(s"Using Cirium Live Base Feed")
      CiriumFeed(config.get[String]("feeds.cirium.host"), portCode).tickingSource(30 seconds)
    }
    else {
      log.info(s"Using Noop Base Live Feed")
      arrivalsNoOp
    }
  }

  def liveArrivalsSource(portCode: PortCode): Source[ArrivalsFeedResponse, Cancellable] =
    portCode.iata match {
      case "LHR" =>
        val host = config.get[String]("feeds.lhr.sftp.live.host")
        val username = config.get[String]("feeds.lhr.sftp.live.username")
        val password = config.get[String]("feeds.lhr.sftp.live.password")
        val contentProvider = () => LhrSftpLiveContentProvider(host, username, password).latestContent
        LHRFlightFeed(contentProvider)
      case "EDI" =>
        createLiveChromaFlightFeed(ChromaLive).chromaEdiFlights()
      case "LGW" =>
        val lgwNamespace = params.maybeLGWNamespace.getOrElse(throw new Exception("Missing LGW Azure Namespace parameter"))
        val lgwSasToKey = params.maybeLGWSASToKey.getOrElse(throw new Exception("Missing LGW SAS Key for To Queue"))
        val lgwServiceBusUri = params.maybeLGWServiceBusUri.getOrElse(throw new Exception("Missing LGW Service Bus Uri"))
        val azureClient = LGWAzureClient(LGWFeed.serviceBusClient(lgwNamespace, lgwSasToKey, lgwServiceBusUri))
        LGWFeed(azureClient)(system).source()
      case "BHX" if params.bhxIataEndPointUrl.nonEmpty =>
        BHXFeed(BHXClient(params.bhxIataUsername, params.bhxIataEndPointUrl), 80 seconds, 1 milliseconds)
      case "BHX" =>
        BHXLiveFeedLegacy(params.maybeBhxSoapEndPointUrl.getOrElse(throw new Exception("Missing BHX live feed URL")))
      case "LCY" if params.lcyLiveEndPointUrl.nonEmpty =>
        LCYFeed(LCYClient(new HttpClient, params.lcyLiveUsername, params.lcyLiveEndPointUrl, params.lcyLiveUsername, params.lcyLivePassword), 80 seconds, 1 milliseconds)
      case "LTN" =>
        val url = params.maybeLtnLiveFeedUrl.getOrElse(throw new Exception("Missing live feed url"))
        val username = params.maybeLtnLiveFeedUsername.getOrElse(throw new Exception("Missing live feed username"))
        val password = params.maybeLtnLiveFeedPassword.getOrElse(throw new Exception("Missing live feed password"))
        val token = params.maybeLtnLiveFeedToken.getOrElse(throw new Exception("Missing live feed token"))
        val timeZone = params.maybeLtnLiveFeedTimeZone match {
          case Some(tz) => DateTimeZone.forID(tz)
          case None => DateTimeZone.UTC
        }
        val requester = LtnFeedRequester(url, token, username, password)
        LtnLiveFeed(requester, timeZone).tickingSource(30 seconds)
      case "MAN" | "STN" | "EMA" =>
        if (config.get[Boolean]("feeds.mag.use-legacy")) {
          log.info(s"Using legacy MAG live feed")
          createLiveChromaFlightFeed(ChromaLive).chromaVanillaFlights(30 seconds)
        } else {
          log.info(s"Using new MAG live feed")
          val privateKey: String = config.get[String]("feeds.mag.private-key")
          val claimIss: String = config.get[String]("feeds.mag.claim.iss")
          val claimRole: String = config.get[String]("feeds.mag.claim.role")
          val claimSub: String = config.get[String]("feeds.mag.claim.sub")
          MagFeed(privateKey, claimIss, claimRole, claimSub, now, airportConfig.portCode, ProdFeedRequester).tickingSource
        }
      case "GLA" =>
        val liveUrl = params.maybeGlaLiveUrl.getOrElse(throw new Exception("Missing GLA Live Feed Url"))
        val livePassword = params.maybeGlaLivePassword.getOrElse(throw new Exception("Missing GLA Live Feed Password"))
        val liveToken = params.maybeGlaLiveToken.getOrElse(throw new Exception("Missing GLA Live Feed Token"))
        val liveUsername = params.maybeGlaLiveUsername.getOrElse(throw new Exception("Missing GLA Live Feed Username"))
        GlaFeed(liveUrl, liveToken, livePassword, liveUsername, ProdGlaFeedRequester).tickingSource
      case _ => arrivalsNoOp
    }

  def forecastArrivalsSource(portCode: PortCode): Source[ArrivalsFeedResponse, Cancellable] = {
    val feed = portCode match {
      case PortCode("LHR") | PortCode("LGW") | PortCode("STN") => createArrivalFeed
      case PortCode("BHX") => BHXForecastFeedLegacy(params.maybeBhxSoapEndPointUrl.getOrElse(throw new Exception("Missing BHX feed URL")))
      case _ => system.log.info(s"No Forecast Feed defined.")
        arrivalsNoOp
    }
    feed
  }

  def createLiveChromaFlightFeed(feedType: ChromaFeedType): ChromaLiveFeed = {
    ChromaLiveFeed(new ChromaFetcher[ChromaLiveFlight](feedType, ChromaFlightMarshallers.live) with ProdSendAndReceive)
  }

  def createForecastChromaFlightFeed(feedType: ChromaFeedType): ChromaForecastFeed = {
    ChromaForecastFeed(new ChromaFetcher[ChromaForecastFlight](feedType, ChromaFlightMarshallers.forecast) with ProdSendAndReceive)
  }

  def createArrivalFeed: Source[ArrivalsFeedResponse, Cancellable] = {
    implicit val timeout: Timeout = new Timeout(10 seconds)
    val arrivalFeed = ArrivalFeed(arrivalsImportActor)
    Source
      .tick(10 seconds, 5 seconds, NotUsed)
      .mapAsync(1)(_ => arrivalFeed.requestFeed)
  }

  def initialState[A](askableActor: ActorRef): Option[A] = Await.result(initialStateFuture[A](askableActor), 2 minutes)

  def initialFlightsPortState(actor: ActorRef, forecastMaxDays: Int): Future[Option[PortState]] = {
    val from = now().getLocalLastMidnight.addDays(-1)
    val to = from.addDays(forecastMaxDays)
    val request = GetFlights(from.millisSinceEpoch, to.millisSinceEpoch)
    FlightsRouterActor.runAndCombine(actor
      .ask(request)(new Timeout(15 seconds)).mapTo[Source[FlightsWithSplits, NotUsed]])
      .map { fws =>
        Option(PortState(fws.flights.values, Iterable(), Iterable()))
      }
  }

  def initialStateFuture[A](askableActor: ActorRef): Future[Option[A]] = {
    val actorPath = askableActor.actorRef.path
    queryActorWithRetry[A](askableActor, GetState)
      .map {
        case Some(state: A) if state.isInstanceOf[A] =>
          log.debug(s"Got initial state (Some(${state.getClass})) from $actorPath")
          Option(state)
        case None =>
          log.warn(s"Got no state (None) from $actorPath")
          None
      }
      .recoverWith {
        case t =>
          log.error(s"Failed to get response from $askableActor", t)
          Future(None)
      }
  }

  def queryActorWithRetry[A](actor: ActorRef, toAsk: Any): Future[Option[A]] = {
    val future = actor.ask(toAsk)(new Timeout(2 minutes)).map {
      case Some(state: A) if state.isInstanceOf[A] => Option(state)
      case state: A if !state.isInstanceOf[Option[A]] => Option(state)
      case _ => None
    }

    implicit val scheduler: Scheduler = system.scheduler
    Retry.retry(future, RetryDelays.fibonacci, 3, 5 seconds)
  }

  def getFeedStatus: Future[Seq[FeedSourceStatuses]] = Source(feedActorsForPort)
    .mapAsync(1) {
      case (_, actor) => queryActorWithRetry[FeedSourceStatuses](actor, GetFeedStatuses)
    }
    .collect { case Some(fs) => fs }
    .withAttributes(StreamSupervision.resumeStrategyWithLog("getFeedStatus"))
    .runWith(Sink.seq)
}
