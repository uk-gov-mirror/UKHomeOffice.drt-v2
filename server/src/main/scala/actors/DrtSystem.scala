package actors

import actors.Sizes.oneMegaByte
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Cancellable, Props, Scheduler}
import akka.pattern.AskableActorRef
import akka.stream.scaladsl.{Source, SourceQueueWithComplete}
import akka.stream.{Materializer, OverflowStrategy}
import akka.util.Timeout
import com.amazonaws.auth.AWSCredentials
import com.typesafe.config.ConfigFactory
import controllers.{Deskstats, PaxFlow, UserRoleProviderLike}
import drt.chroma._
import drt.chroma.chromafetcher.{ChromaFetcher, ChromaFetcherForecast}
import drt.http.ProdSendAndReceive
import drt.server.feeds.acl.AclFeed
import drt.server.feeds.api.S3ApiProvider
import drt.server.feeds.bhx.{BHXForecastFeed, BHXLiveFeed}
import drt.server.feeds.chroma.{ChromaForecastFeed, ChromaLiveFeed}
import drt.server.feeds.lgw.{LGWFeed, LGWForecastFeed}
import drt.server.feeds.lhr.live.LegacyLhrLiveContentProvider
import drt.server.feeds.lhr.sftp.LhrSftpLiveContentProvider
import drt.server.feeds.lhr.{LHRFlightFeed, LHRForecastFeed}
import drt.server.feeds.ltn.LtnLiveFeed
import drt.server.feeds.{ArrivalsFeedResponse, ArrivalsFeedSuccess, ManifestsFeedResponse}
import drt.shared.CrunchApi.{MillisSinceEpoch, PortState}
import drt.shared.FlightsApi.{Flights, TerminalName}
import drt.shared._
import manifests.ManifestLookup
import manifests.actors.{RegisteredArrivals, RegisteredArrivalsActor}
import manifests.graph.{BatchStage, LookupStage, ManifestsGraph}
import manifests.passengers.S3ManifestPoller
import org.apache.spark.sql.SparkSession
import org.joda.time.DateTimeZone
import play.api.Configuration
import play.api.mvc.{Headers, Session}
import services.PcpArrival.{GateOrStandWalkTime, gateOrStandWalkTimeCalculator, walkTimeMillisProviderFromCsv}
import services.SplitsProvider.SplitProvider
import services._
import services.crunch._
import services.graphstages.Crunch.{oneDayMillis, oneMinuteMillis}
import services.graphstages._
import services.prediction.SparkSplitsPredictorFactory
import slickdb.{ArrivalTable, Tables, VoyageManifestPassengerInfoTable}

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

trait DrtSystemInterface extends UserRoleProviderLike {
  val now: () => SDateLike = () => SDate.now()

  val liveCrunchStateActor: ActorRef
  val forecastCrunchStateActor: ActorRef
  val shiftsActor: ActorRef
  val fixedPointsActor: ActorRef
  val staffMovementsActor: ActorRef
  val alertsActor: ActorRef
  val arrivalsImportActor: ActorRef

  val aclFeed: AclFeed

  def run(): Unit

  def getFeedStatus: Future[Seq[FeedStatuses]]
}

object DrtStaticParameters {
  val expireAfterMillis: MillisSinceEpoch = 2 * oneDayMillis

  def time48HoursAgo(now: () => SDateLike): () => SDateLike = () => now().addDays(-2)

  def timeBeforeThisMonth(now: () => SDateLike): () => SDateLike = () => now().startOfTheMonth()
}

object PostgresTables extends {
  val profile = slick.jdbc.PostgresProfile
} with Tables

case class DrtConfigParameters(config: Configuration) {
  val maxDaysToCrunch: Int = config.get[Int]("crunch.forecast.max_days")
  val aclPollMinutes: Int = config.get[Int]("crunch.forecast.poll_minutes")
  val snapshotIntervalVm: Int = config.get[Int]("persistence.snapshot-interval.voyage-manifest")
  val snapshotMegaBytesBaseArrivals: Int = (config.get[Double]("persistence.snapshot-megabytes.base-arrivals") * oneMegaByte).toInt
  val snapshotMegaBytesFcstArrivals: Int = (config.get[Double]("persistence.snapshot-megabytes.forecast-arrivals") * oneMegaByte).toInt
  val snapshotMegaBytesLiveArrivals: Int = (config.get[Double]("persistence.snapshot-megabytes.live-arrivals") * oneMegaByte).toInt
  val snapshotMegaBytesFcstPortState: Int = (config.get[Double]("persistence.snapshot-megabytes.forecast-portstate") * oneMegaByte).toInt
  val snapshotMegaBytesLivePortState: Int = (config.get[Double]("persistence.snapshot-megabytes.live-portstate") * oneMegaByte).toInt
  val snapshotMegaBytesVoyageManifests: Int = (config.get[Double]("persistence.snapshot-megabytes.voyage-manifest") * oneMegaByte).toInt
  val awSCredentials: AWSCredentials = new AWSCredentials {
    override def getAWSAccessKeyId: TerminalName = config.get[String]("aws.credentials.access_key_id")

    override def getAWSSecretKey: TerminalName = config.get[String]("aws.credentials.secret_key")
  }
  val ftpServer: String = config.get[String]("acl.host")
  val username: String = config.get[String]("acl.username")
  val path: String = config.get[String]("acl.keypath")
  val refreshArrivalsOnStart: Boolean = config.get[Boolean]("crunch.refresh-arrivals-on-start")
  val recrunchOnStart: Boolean = config.get[Boolean]("crunch.recrunch-on-start")
  val resetRegisteredArrivalOnStart: Boolean = config.get[Boolean]("crunch.manifests.reset-registered-arrivals-on-start")
  val useNationalityBasedProcessingTimes: Boolean = config.get[Boolean]("feature-flags.nationality-based-processing-times")

  val manifestLookupBatchSize: Int = config.get[Int]("crunch.manifests.lookup-batch-size")

  val useLegacyManifests: Boolean = config.get[Boolean]("feature-flags.use-legacy-manifests")
  val useSplitsPrediction: Boolean = config.get[Boolean]("feature-flags.use-splits-prediction")
  val rawSplitsUrl: String = config.get[String]("crunch.splits.raw-data-path")
  val dqZipBucketName: String = config.get[String]("dq.s3.bucket")
  val apiS3PollFrequencyMillis: MillisSinceEpoch = config.get[Int]("dq.s3.poll_frequency_seconds") * 1000L
  val isSuperUserMode: Boolean = config.get[Boolean]("feature-flags.super-user-mode")
  val maybeBlackJackUrl: Option[String] = config.get[String]("feeds.lhr.blackjack_url") match {
    case url if url.nonEmpty => Option(url)
    case _ => None
  }

  val bhxSoapEndPointUrl: String = config.get[String]("feeds.bhx.soap.endPointUrl")

  val b5JStartDate: String = "2019-06-01"

  val ltnLiveFeedUrl: String = config.get[String]("feeds.ltn.live.url")
  val ltnLiveFeedUsername: String = config.get[String]("feeds.ltn.live.username")
  val ltnLiveFeedPassword: String = config.get[String]("feeds.ltn.live.password")
  val ltnLiveFeedToken: String = config.get[String]("feeds.ltn.live.token")
  val ltnLiveFeedTimeZone: String = config.get[String]("feeds.ltn.live.timezone")

  val lgwNamespace: String = config.get[String]("feeds.lgw.live.azure.namespace")
  val lgwSasToKey: String = config.get[String]("feeds.lgw.live.azure.sas_to_Key")
  val lgwServiceBusUri: String = config.get[String]("feeds.lgw.live.azure.service_bus_uri")
}

case class SubscribeRequestQueue(subscriber: SourceQueueWithComplete[List[Arrival]])

case class SubscribeResponseQueue(subscriber: SourceQueueWithComplete[ManifestsFeedResponse])

case class DrtSystem(actorSystem: ActorSystem, config: Configuration, airportConfig: AirportConfig)
                    (implicit actorMaterializer: Materializer) extends DrtSystemInterface {

  implicit val system: ActorSystem = actorSystem

  val params = DrtConfigParameters(config)

  import DrtStaticParameters._

  system.log.info(s"recrunchOnStart: ${params.recrunchOnStart}")

  val aclFeed = AclFeed(params.ftpServer, params.username, params.path, airportConfig.feedPortCode, aclTerminalMapping(airportConfig.portCode))

  system.log.info(s"Path to splits file ${ConfigFactory.load.getString("passenger_splits_csv_url")}")

  val gateWalkTimesProvider: GateOrStandWalkTime = walkTimeMillisProviderFromCsv(ConfigFactory.load.getString("walk_times.gates_csv_url"))
  val standWalkTimesProvider: GateOrStandWalkTime = walkTimeMillisProviderFromCsv(ConfigFactory.load.getString("walk_times.stands_csv_url"))

  val aggregateArrivalsDbConfigKey = "aggregated-db"

  val purgeOldLiveSnapshots = false
  val purgeOldForecastSnapshots = true

  val optimiserService: OptimiserLike = if (config.get[Boolean]("crunch.optimiser-service.use-local")) {
    OptimiserLocal
  } else {
    val optimiserServiceHost: String = config.get[String]("crunch.optimiser-service.host")
    val optimiserServicePort: String = config.get[String]("crunch.optimiser-service.port")
    Optimiser(OptimiserService(optimiserServiceHost, optimiserServicePort).uri)
  }

  val liveCrunchStateProps = Props(classOf[CrunchStateActor], Option(airportConfig.portStateSnapshotInterval), params.snapshotMegaBytesLivePortState, "crunch-state", airportConfig.queues, now, expireAfterMillis, purgeOldLiveSnapshots)
  val forecastCrunchStateProps = Props(classOf[CrunchStateActor], Option(100), params.snapshotMegaBytesFcstPortState, "forecast-crunch-state", airportConfig.queues, now, expireAfterMillis, purgeOldForecastSnapshots)

  lazy val baseArrivalsActor: ActorRef = system.actorOf(Props(classOf[ForecastBaseArrivalsActor], params.snapshotMegaBytesBaseArrivals, now, expireAfterMillis), name = "base-arrivals-actor")
  lazy val forecastArrivalsActor: ActorRef = system.actorOf(Props(classOf[ForecastPortArrivalsActor], params.snapshotMegaBytesFcstArrivals, now, expireAfterMillis), name = "forecast-arrivals-actor")
  lazy val liveArrivalsActor: ActorRef = system.actorOf(Props(classOf[LiveArrivalsActor], params.snapshotMegaBytesLiveArrivals, now, expireAfterMillis), name = "live-arrivals-actor")

  lazy val arrivalsImportActor: ActorRef = system.actorOf(Props(classOf[ArrivalsImportActor]), name = "arrivals-import-actor")

  lazy val aggregatedArrivalsActor: ActorRef = system.actorOf(Props(classOf[AggregatedArrivalsActor], airportConfig.portCode, ArrivalTable(airportConfig.portCode, PostgresTables)), name = "aggregated-arrivals-actor")
  lazy val registeredArrivalsActor: ActorRef = system.actorOf(Props(classOf[RegisteredArrivalsActor], oneMegaByte, None, airportConfig.portCode, now, expireAfterMillis), name = "registered-arrivals-actor")

  lazy val queueLoadActor: ActorRef = system.actorOf(Props(classOf[QueueLoadActor], now, expireAfterMillis), name = "queue-load-actor")

  lazy val liveCrunchStateActor: ActorRef = system.actorOf(liveCrunchStateProps, name = "crunch-live-state-actor")
  lazy val forecastCrunchStateActor: ActorRef = system.actorOf(forecastCrunchStateProps, name = "crunch-forecast-state-actor")

  lazy val voyageManifestsActor: ActorRef = system.actorOf(Props(classOf[VoyageManifestsActor], params.snapshotMegaBytesVoyageManifests, now, expireAfterMillis, Option(params.snapshotIntervalVm)), name = "voyage-manifests-actor")
  lazy val lookup = ManifestLookup(VoyageManifestPassengerInfoTable(PostgresTables))
  lazy val voyageManifestsRequestActor: ActorRef = system.actorOf(Props(classOf[VoyageManifestsRequestActor], airportConfig.portCode, lookup), name = "voyage-manifests-request-actor")

  lazy val manifestsArrivalRequestSource: Source[List[Arrival], SourceQueueWithComplete[List[Arrival]]] = Source.queue[List[Arrival]](100, OverflowStrategy.backpressure)

  lazy val shiftsActor: ActorRef = system.actorOf(Props(classOf[ShiftsActor], now, timeBeforeThisMonth(now)))
  lazy val fixedPointsActor: ActorRef = system.actorOf(Props(classOf[FixedPointsActor], now))
  lazy val staffMovementsActor: ActorRef = system.actorOf(Props(classOf[StaffMovementsActor], now, time48HoursAgo(now)))

  lazy val alertsActor: ActorRef = system.actorOf(Props(classOf[AlertsActor]))
  val historicalSplitsProvider: SplitProvider = SplitsProvider.csvProvider
  val splitsPredictorStage: SplitsPredictorBase = createSplitsPredictionStage(params.useSplitsPrediction, params.rawSplitsUrl)

  val s3ApiProvider = S3ApiProvider(params.awSCredentials, params.dqZipBucketName)
  val initialManifestsState: Option[VoyageManifestState] = initialState(voyageManifestsActor, GetState)
  val latestZipFileName: String = initialManifestsState.map(_.latestZipFilename).getOrElse("")

  lazy val voyageManifestsLiveSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]] = Source.queue[ManifestsFeedResponse](1, OverflowStrategy.backpressure)
  lazy val voyageManifestsHistoricSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]] = Source.queue[ManifestsFeedResponse](1, OverflowStrategy.backpressure)

  system.log.info(s"useNationalityBasedProcessingTimes: ${params.useNationalityBasedProcessingTimes}")
  system.log.info(s"useSplitsPrediction: ${params.useSplitsPrediction}")

  def getRoles(config: Configuration, headers: Headers, session: Session): Set[Role] =
    if (params.isSuperUserMode) {
      system.log.info(s"Using Super User Roles")
      Roles.availableRoles
    } else userRolesFromHeader(headers)

  def run(): Unit = {
    val futurePortStates: Future[(Option[PortState], Option[PortState], Option[Set[Arrival]], Option[Set[Arrival]], Option[Set[Arrival]], Option[RegisteredArrivals])] = {
      val maybeLivePortState = initialStateFuture[PortState](liveCrunchStateActor, GetState)
      val maybeForecastPortState = initialStateFuture[PortState](forecastCrunchStateActor, GetState)
      val maybeInitialBaseArrivals = initialStateFuture[ArrivalsState](baseArrivalsActor, GetState).map(_.map(_.arrivals.values.toSet))
      val maybeInitialFcstArrivals = initialStateFuture[ArrivalsState](forecastArrivalsActor, GetState).map(_.map(_.arrivals.values.toSet))
      val maybeInitialLiveArrivals = initialStateFuture[ArrivalsState](liveArrivalsActor, GetState).map(_.map(_.arrivals.values.toSet))
      val maybeInitialRegisteredArrivals = initialStateFuture[RegisteredArrivals](registeredArrivalsActor, GetState)
      for {
        lps <- maybeLivePortState
        fps <- maybeForecastPortState
        ba <- maybeInitialBaseArrivals
        fa <- maybeInitialFcstArrivals
        la <- maybeInitialLiveArrivals
        ra <- maybeInitialRegisteredArrivals
      } yield (lps, fps, ba, fa, la, ra)
    }

    futurePortStates.onComplete {
      case Success((maybeLiveState, maybeForecastState, maybeBaseArrivals, maybeForecastArrivals, maybeLiveArrivals, maybeRegisteredArrivals)) =>
        system.log.info(s"Successfully restored initial state for App")
        val initialPortState: Option[PortState] = mergePortStates(maybeForecastState, maybeLiveState)

        val crunchInputs: CrunchSystem[Cancellable] = startCrunchSystem(initialPortState, maybeBaseArrivals, maybeForecastArrivals, maybeLiveArrivals, params.recrunchOnStart, params.refreshArrivalsOnStart, true)

        if (maybeRegisteredArrivals.isDefined) log.info(s"sending ${maybeRegisteredArrivals.get.arrivals.size} initial registered arrivals to batch stage")
        else log.info(s"sending no registered arrivals to batch stage")

        new S3ManifestPoller(crunchInputs.manifestsLiveResponse, airportConfig.portCode, latestZipFileName, s3ApiProvider).startPollingForManifests()

        if (!params.useLegacyManifests) {
          val initialRegisteredArrivals = if (params.resetRegisteredArrivalOnStart) {
            log.info(s"Resetting registered arrivals for manifest lookups")
            val maybeAllArrivals: Option[SortedMap[ArrivalKey, Option[Long]]] = initialPortState
              .map(state => SortedMap[ArrivalKey, Option[Long]]() ++ state.flights.values.map(fws => (ArrivalKey(fws.apiFlight), None)))
            Option(RegisteredArrivals(maybeAllArrivals.getOrElse(SortedMap())))
          } else maybeRegisteredArrivals
          val manifestsSourceQueue = startManifestsGraph(initialRegisteredArrivals)
          voyageManifestsRequestActor ! SubscribeRequestQueue(manifestsSourceQueue)
          voyageManifestsRequestActor ! SubscribeResponseQueue(crunchInputs.manifestsHistoricResponse)
        }

        subscribeStaffingActors(crunchInputs)
        startScheduledFeedImports(crunchInputs)

      case Failure(error) =>
        system.log.error(s"Failed to restore initial state for App", error)
        System.exit(1)
    }
  }

  override def getFeedStatus: Future[Seq[FeedStatuses]] = {
    val actors: Seq[AskableActorRef] = Seq(liveArrivalsActor, forecastArrivalsActor, baseArrivalsActor, voyageManifestsActor)

    val statuses: Seq[Future[Option[FeedStatuses]]] = actors.map(a => initialStateFuture[FeedStatuses](a, GetFeedStatuses))

    Future
      .sequence(statuses)
      .map(maybeStatuses => maybeStatuses.collect { case Some(fs) => fs })
  }

  def aclTerminalMapping(portCode: String): TerminalName => TerminalName = portCode match {
    case "LGW" => (tIn: TerminalName) => Map("1I" -> "S", "2I" -> "N").getOrElse(tIn, "")
    case "MAN" => (tIn: TerminalName) => Map("T1" -> "T1", "T2" -> "T2", "T3" -> "T3").getOrElse(tIn, "")
    case "EMA" => (tIn: TerminalName) => Map("1I" -> "T1", "1D" -> "T1").getOrElse(tIn, "")
    case "EDI" => (tIn: TerminalName) => Map("1I" -> "A1").getOrElse(tIn, "")
    case _ => (tIn: TerminalName) => s"T${tIn.take(1)}"
  }

  def startScheduledFeedImports(crunchInputs: CrunchSystem[Cancellable]): Unit = {
    if (airportConfig.feedPortCode == "LHR") params.maybeBlackJackUrl.map(csvUrl => {
      val requestIntervalMillis = 5 * oneMinuteMillis
      Deskstats.startBlackjack(csvUrl, crunchInputs.actualDeskStats, requestIntervalMillis milliseconds, () => SDate.now().addDays(-1))
    })
  }

  def subscribeStaffingActors(crunchInputs: CrunchSystem[Cancellable]): Unit = {
    shiftsActor ! AddShiftSubscribers(List(crunchInputs.shifts))
    fixedPointsActor ! AddFixedPointSubscribers(List(crunchInputs.fixedPoints))
    staffMovementsActor ! AddStaffMovementsSubscribers(List(crunchInputs.staffMovements))
  }

  def startManifestsGraph(maybeRegisteredArrivals: Option[RegisteredArrivals]): SourceQueueWithComplete[List[Arrival]] = {
    val minimumRefreshIntervalMillis = 15 * 60 * 1000

    lazy val batchStage: BatchStage = new BatchStage(now, Crunch.isDueLookup, params.manifestLookupBatchSize, expireAfterMillis, maybeRegisteredArrivals, minimumRefreshIntervalMillis)
    lazy val lookupStage: LookupStage = new LookupStage(airportConfig.portCode, lookup)

    ManifestsGraph(manifestsArrivalRequestSource, batchStage, lookupStage, voyageManifestsRequestActor, registeredArrivalsActor).run
  }

  def startCrunchSystem(initialPortState: Option[PortState],
                        initialBaseArrivals: Option[Set[Arrival]],
                        initialForecastArrivals: Option[Set[Arrival]],
                        initialLiveArrivals: Option[Set[Arrival]],
                        recrunchOnStart: Boolean,
                        refreshArrivalsOnStart: Boolean,
                        checkRequiredStaffUpdatesOnStartup: Boolean): CrunchSystem[Cancellable] = {

    val crunchInputs = CrunchSystem(CrunchProps(
      airportConfig = airportConfig,
      pcpArrival = pcpArrivalTimeCalculator,
      historicalSplitsProvider = historicalSplitsProvider,
      liveCrunchStateActor = liveCrunchStateActor,
      forecastCrunchStateActor = forecastCrunchStateActor,
      maxDaysToCrunch = params.maxDaysToCrunch,
      expireAfterMillis = expireAfterMillis,
      actors = Map(
        "shifts" -> shiftsActor,
        "fixed-points" -> fixedPointsActor,
        "staff-movements" -> staffMovementsActor,
        "base-arrivals" -> baseArrivalsActor,
        "forecast-arrivals" -> forecastArrivalsActor,
        "live-arrivals" -> liveArrivalsActor,
        "aggregated-arrivals" -> aggregatedArrivalsActor,
        "queue-load" -> queueLoadActor
      ),
      useNationalityBasedProcessingTimes = params.useNationalityBasedProcessingTimes,
      useLegacyManifests = params.useLegacyManifests,
      splitsPredictorStage = splitsPredictorStage,
      b5JStartDate = SDate(params.b5JStartDate),
      manifestsLiveSource = voyageManifestsLiveSource,
      manifestsHistoricSource = voyageManifestsHistoricSource,
      voyageManifestsActor = voyageManifestsActor,
      voyageManifestsRequestActor = voyageManifestsRequestActor,
      optimiser = optimiserService,
      simulator = TryRenjin.runSimulationOfWork,
      initialPortState = initialPortState,
      initialBaseArrivals = initialBaseArrivals.getOrElse(Set()),
      initialFcstArrivals = initialForecastArrivals.getOrElse(Set()),
      initialLiveArrivals = initialLiveArrivals.getOrElse(Set()),
      arrivalsBaseSource = baseArrivalsSource(),
      arrivalsFcstSource = forecastArrivalsSource(airportConfig.feedPortCode),
      arrivalsLiveSource = liveArrivalsSource(airportConfig.feedPortCode),
      initialShifts = initialState(shiftsActor, GetState).getOrElse(ShiftAssignments(Seq())),
      initialFixedPoints = initialState(fixedPointsActor, GetState).getOrElse(FixedPointAssignments(Seq())),
      initialStaffMovements = initialState[StaffMovements](staffMovementsActor, GetState).map(_.movements).getOrElse(Seq[StaffMovement]()),
      recrunchOnStart = recrunchOnStart,
      refreshArrivalsOnStart = refreshArrivalsOnStart,
      checkRequiredStaffUpdatesOnStartup = checkRequiredStaffUpdatesOnStartup
    ))
    crunchInputs
  }

  def initialState[A](askableActor: AskableActorRef, toAsk: Any): Option[A] = Await.result(initialStateFuture[A](askableActor, toAsk), 5 minutes)

  def initialStateFuture[A](askableActor: AskableActorRef, toAsk: Any): Future[Option[A]] = {
    val future = askableActor.ask(toAsk)(new Timeout(5 minutes)).map {
      case Some(state: A) if state.isInstanceOf[A] =>
        log.info(s"Got initial state (Some(${state.getClass})) from ${askableActor.toString}")
        Option(state)
      case state: A if !state.isInstanceOf[Option[A]] =>
        log.info(s"Got initial state (${state.getClass}) from ${askableActor.toString}")
        Option(state)
      case None =>
        log.info(s"Got no state (None) from ${askableActor.toString}")
        None
      case _ =>
        log.info(s"Got unexpected GetState response from ${askableActor.toString}")
        None
    }

    implicit val scheduler: Scheduler = actorSystem.scheduler
    Retry.retry(future, RetryDelays.fibonacci, 3, 5 seconds)
  }

  def mergePortStates(maybeForecastPs: Option[PortState],
                      maybeLivePs: Option[PortState]): Option[PortState] = (maybeForecastPs, maybeLivePs) match {
    case (None, None) => None
    case (Some(fps), None) => Option(fps)
    case (None, Some(lps)) => Option(lps)
    case (Some(fps), Some(lps)) =>
      Option(PortState(
        fps.flights ++ lps.flights,
        fps.crunchMinutes ++ lps.crunchMinutes,
        fps.staffMinutes ++ lps.staffMinutes))
  }

  def createSplitsPredictionStage(predictSplits: Boolean,
                                  rawSplitsUrl: String): SplitsPredictorBase = if (predictSplits)
    new SplitsPredictorStage(SparkSplitsPredictorFactory(createSparkSession(), rawSplitsUrl, airportConfig.feedPortCode))
  else
    new DummySplitsPredictor()

  def createSparkSession(): SparkSession = {
    SparkSession
      .builder
      .appName("DRT Predictor")
      .config("spark.master", "local")
      .getOrCreate()
  }

  def liveArrivalsSource(portCode: String): Source[ArrivalsFeedResponse, Cancellable] = {
    val feed = portCode match {
      case "LHR" =>
        val contentProvider = if (config.get[Boolean]("feeds.lhr.use-legacy-live")) {
          log.info(s"Using legacy LHR live feed")
          () => LegacyLhrLiveContentProvider().csvContentsProviderProd()
        } else {
          log.info(s"Using new LHR live feed")
          val host = config.get[String]("feeds.lhr.sftp.live.host")
          val username = config.get[String]("feeds.lhr.sftp.live.username")
          val password = config.get[String]("feeds.lhr.sftp.live.password")
          () => LhrSftpLiveContentProvider(host, username, password).latestContent
        }
        LHRFlightFeed(contentProvider)
      case "EDI" => createLiveChromaFlightFeed(ChromaLive).chromaEdiFlights()
      case "LGW" => LGWFeed(params.lgwNamespace, params.lgwSasToKey, params.lgwServiceBusUri)(system).source()
      case "BHX" => BHXLiveFeed(params.bhxSoapEndPointUrl, 60 seconds)
      case "LTN" =>
        val url = params.ltnLiveFeedUrl
        val username = params.ltnLiveFeedUsername
        val password = params.ltnLiveFeedPassword
        val token = params.ltnLiveFeedToken
        val timeZone = DateTimeZone.forID(params.ltnLiveFeedTimeZone)
        LtnLiveFeed(url, token, username, password, timeZone).tickingSource
      case _ => createLiveChromaFlightFeed(ChromaLive).chromaVanillaFlights(30 seconds)
    }
    feed
  }

  def forecastArrivalsSource(portCode: String): Source[ArrivalsFeedResponse, Cancellable] = {
    val forecastNoOp = Source.tick[ArrivalsFeedResponse](100 days, 100 days, ArrivalsFeedSuccess(Flights(Seq()), SDate.now()))
    val feed = portCode match {
      case "LHR" => createForecastLHRFeed()
      case "BHX" => BHXForecastFeed(params.bhxSoapEndPointUrl, 60 minutes)
      case "LGW" => LGWForecastFeed()
      case _ =>
        system.log.info(s"No Forecast Feed defined.")
        forecastNoOp
    }
    feed
  }

  def baseArrivalsSource(): Source[ArrivalsFeedResponse, Cancellable] = Source.tick(1 second, 60 minutes, NotUsed).map(_ => {
    system.log.info(s"Requesting ACL feed")
    aclFeed.requestArrivals
  })

  def walkTimeProvider(flight: Arrival): MillisSinceEpoch =
    gateOrStandWalkTimeCalculator(gateWalkTimesProvider, standWalkTimesProvider, airportConfig.defaultWalkTimeMillis.getOrElse(flight.Terminal, 300000L))(flight)

  def pcpArrivalTimeCalculator: Arrival => MilliDate =
    PaxFlow.pcpArrivalTimeForFlight(airportConfig.timeToChoxMillis, airportConfig.firstPaxOffMillis)(walkTimeProvider)

  def createLiveChromaFlightFeed(feedType: ChromaFeedType): ChromaLiveFeed = {
    ChromaLiveFeed(system.log, new ChromaFetcher(feedType, system) with ProdSendAndReceive)
  }

  def createForecastChromaFlightFeed(feedType: ChromaFeedType): ChromaForecastFeed = {
    ChromaForecastFeed(system.log, new ChromaFetcherForecast(feedType, system) with ProdSendAndReceive)
  }

  def createForecastLHRFeed(): Source[ArrivalsFeedResponse, Cancellable] = {
    val lhrForecastFeed = LHRForecastFeed(arrivalsImportActor)
    Source
      .tick(10 seconds, 60 seconds, NotUsed)
      .map(_ => lhrForecastFeed.requestFeed)
  }
}
