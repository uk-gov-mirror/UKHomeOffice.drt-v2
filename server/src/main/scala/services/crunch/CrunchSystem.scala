package services.crunch

import actors.VoyageManifestState
import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.AskableActorRef
import akka.stream._
import akka.stream.scaladsl.{Source, SourceQueueWithComplete}
import drt.chroma.ArrivalsDiffingStage
import drt.shared.CrunchApi.{CrunchMinutes, PortState, StaffMinutes}
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared.SplitRatiosNs.SplitSources
import drt.shared.{SDateLike, _}
import org.slf4j.{Logger, LoggerFactory}
import passengersplits.core.SplitsCalculator
import queueus._
import server.feeds.{ArrivalsFeedResponse, ManifestsFeedResponse}
import services._
import services.graphstages.Crunch._
import services.graphstages._

import scala.language.postfixOps


case class CrunchSystem[FR](shifts: SourceQueueWithComplete[ShiftAssignments],
                            fixedPoints: SourceQueueWithComplete[FixedPointAssignments],
                            staffMovements: SourceQueueWithComplete[Seq[StaffMovement]],
                            baseArrivalsResponse: FR,
                            forecastArrivalsResponse: FR,
                            liveArrivalsResponse: FR,
                            manifestsLiveResponse: SourceQueueWithComplete[ManifestsFeedResponse],
                            manifestsHistoricResponse: SourceQueueWithComplete[ManifestsFeedResponse],
                            actualDeskStats: SourceQueueWithComplete[ActualDeskStats],
                            killSwitches: List[KillSwitch]
                           )

case class CrunchProps[FR](logLabel: String = "",
                           airportConfig: AirportConfig,
                           pcpArrival: Arrival => MilliDate,
                           historicalSplitsProvider: SplitsProvider.SplitProvider,
                           liveCrunchStateActor: ActorRef,
                           forecastCrunchStateActor: ActorRef,
                           maxDaysToCrunch: Int,
                           expireAfterMillis: Long,
                           minutesToCrunch: Int = 1440,
                           crunchOffsetMillis: Long = 0,
                           actors: Map[String, AskableActorRef],
                           useNationalityBasedProcessingTimes: Boolean,
                           useLegacyManifests: Boolean = false,
                           now: () => SDateLike = () => SDate.now(),
                           initialFlightsWithSplits: Option[FlightsWithSplits] = None,
                           splitsPredictorStage: SplitsPredictorBase,
                           b5JStartDate: SDateLike,
                           manifestsLiveSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]],
                           manifestsHistoricSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]],
                           voyageManifestsActor: ActorRef,
                           voyageManifestsRequestActor: ActorRef,
                           cruncher: TryCrunch,
                           simulator: Simulator,
                           initialPortState: Option[PortState] = None,
                           initialBaseArrivals: Set[Arrival] = Set(),
                           initialFcstArrivals: Set[Arrival] = Set(),
                           initialLiveArrivals: Set[Arrival] = Set(),
                           initialManifestsState: Option[VoyageManifestState],
                           arrivalsBaseSource: Source[ArrivalsFeedResponse, FR],
                           arrivalsFcstSource: Source[ArrivalsFeedResponse, FR],
                           arrivalsLiveSource: Source[ArrivalsFeedResponse, FR],
                           initialShifts: ShiftAssignments = ShiftAssignments(Seq()),
                           initialFixedPoints: FixedPointAssignments = FixedPointAssignments(Seq()),
                           initialStaffMovements: Seq[StaffMovement] = Seq(),
                           recrunchOnStart: Boolean = false,
                           checkRequiredStaffUpdatesOnStartup: Boolean)

object CrunchSystem {

  val log: Logger = LoggerFactory.getLogger(getClass)

  def crunchStartWithOffset(offsetMinutes: Int)(minuteInQuestion: SDateLike): SDateLike = {
    val adjustedMinute = minuteInQuestion.addMinutes(-offsetMinutes)
    Crunch.getLocalLastMidnight(MilliDate(adjustedMinute.millisSinceEpoch)).addMinutes(offsetMinutes)
  }

  def apply[FR](props: CrunchProps[FR])
               (implicit system: ActorSystem, materializer: Materializer): CrunchSystem[FR] = {

    val shiftsSource: Source[ShiftAssignments, SourceQueueWithComplete[ShiftAssignments]] = Source.queue[ShiftAssignments](10, OverflowStrategy.backpressure)
    val fixedPointsSource: Source[FixedPointAssignments, SourceQueueWithComplete[FixedPointAssignments]] = Source.queue[FixedPointAssignments](10, OverflowStrategy.backpressure)
    val staffMovementsSource: Source[Seq[StaffMovement], SourceQueueWithComplete[Seq[StaffMovement]]] = Source.queue[Seq[StaffMovement]](10, OverflowStrategy.backpressure)
    val actualDesksAndQueuesSource: Source[ActualDeskStats, SourceQueueWithComplete[ActualDeskStats]] = Source.queue[ActualDeskStats](10, OverflowStrategy.backpressure)

    val splitsCalculator = SplitsCalculator(props.airportConfig.feedPortCode, props.historicalSplitsProvider, props.airportConfig.defaultPaxSplits.splits.toSet)
    val groupFlightsByCodeShares = CodeShares.uniqueArrivalsWithCodeShares((f: ApiFlightWithSplits) => f.apiFlight) _
    val crunchStartDateProvider: SDateLike => SDateLike = crunchStartWithOffset(props.airportConfig.crunchOffsetMinutes)

    val maybeStaffMinutes = initialStaffMinutesFromPortState(props.initialPortState)
    val maybeCrunchMinutes = initialCrunchMinutesFromPortState(props.initialPortState)

    val initialFlightsWithSplits = initialFlightsFromPortState(props.initialPortState, props.recrunchOnStart)

    val arrivalsStage = new ArrivalsGraphStage(
      name = props.logLabel,
      initialBaseArrivals = props.initialBaseArrivals,
      initialForecastArrivals = props.initialFcstArrivals,
      initialLiveArrivals = props.initialLiveArrivals,
      initialMergedArrivals = initialFlightsWithSplits.map(_.flights.map(fws => (fws.apiFlight.uniqueId, fws.apiFlight)).toMap).getOrElse(Map()),
      pcpArrivalTime = props.pcpArrival,
      validPortTerminals = props.airportConfig.terminalNames.toSet,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now)

    val fcstArrivalsDiffingStage = new ArrivalsDiffingStage(props.initialFcstArrivals.toSeq)
    val liveArrivalsDiffingStage = new ArrivalsDiffingStage(props.initialLiveArrivals.toSeq)

    val arrivalSplitsGraphStage = if (props.useLegacyManifests)
      new ArrivalSplitsFromAllSourcesGraphStage(
        name = props.logLabel,
        optionalInitialFlights = if (props.recrunchOnStart) None else initialFlightsWithSplits,
        optionalInitialManifests = props.initialManifestsState.map(_.manifests),
        splitsCalculator = splitsCalculator,
        groupFlightsByCodeShares = groupFlightsByCodeShares,
        expireAfterMillis = props.expireAfterMillis,
        now = props.now,
        maxDaysToCrunch = props.maxDaysToCrunch)
    else {
      val ptqa = if (props.airportConfig.portCode == "LHR")
        PaxTypeQueueAllocation(
          B5JPlusWithTransitTypeAllocator(props.b5JStartDate),
          TerminalQueueAllocatorWithFastTrack(props.airportConfig.terminalPaxTypeQueueAllocation)
        )
      else
        PaxTypeQueueAllocation(
          B5JPlusTypeAllocator(props.b5JStartDate),
          TerminalQueueAllocator(props.airportConfig.terminalPaxTypeQueueAllocation)
        )

      new ArrivalSplitsGraphStage(
        name = props.logLabel,
        props.airportConfig.portCode,
        optionalInitialFlights = initialFlightsWithSplits,
        splitsCalculator = manifests.queues.SplitsCalculator(
          props.airportConfig.feedPortCode,
          ptqa,
          props.airportConfig.defaultPaxSplits.splits.toSet
        ),
        groupFlightsByCodeShares = groupFlightsByCodeShares,
        expireAfterMillis = props.expireAfterMillis,
        now = props.now,
        maxDaysToCrunch = props.maxDaysToCrunch)
    }

    val splitsPredictorStage = props.splitsPredictorStage

    val staffGraphStage = new StaffGraphStage(
      name = props.logLabel,
      initialShifts = props.initialShifts,
      initialFixedPoints = props.initialFixedPoints,
      optionalInitialMovements = Option(props.initialStaffMovements),
      initialStaffMinutes = maybeStaffMinutes.getOrElse(StaffMinutes(Seq())),
      now = props.now,
      expireAfterMillis = props.expireAfterMillis,
      airportConfig = props.airportConfig,
      numberOfDays = props.maxDaysToCrunch,
      checkRequiredUpdatesOnStartup = props.checkRequiredStaffUpdatesOnStartup)

    val staffBatcher = new StaffBatchUpdateGraphStage(props.now, props.expireAfterMillis, props.airportConfig.crunchOffsetMinutes)
    val loadBatcher = new BatchLoadsByCrunchPeriodGraphStage(props.now, props.expireAfterMillis, crunchStartDateProvider)

    val workloadGraphStage = new WorkloadGraphStage(
      name = props.logLabel,
      optionalInitialLoads = if (props.recrunchOnStart) None else initialLoadsFromPortState(props.initialPortState),
      optionalInitialFlightsWithSplits = initialFlightsWithSplits,
      airportConfig = props.airportConfig,
      natProcTimes = props.airportConfig.nationalityBasedProcTimes,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now,
      useNationalityBasedProcessingTimes = props.useNationalityBasedProcessingTimes)

    val crunchLoadGraphStage = new CrunchLoadGraphStage(
      name = props.logLabel,
      optionalInitialCrunchMinutes = maybeCrunchMinutes,
      airportConfig = props.airportConfig,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now,
      crunch = props.cruncher,
      crunchPeriodStartMillis = crunchStartDateProvider,
      minutesToCrunch = props.minutesToCrunch)

    val simulationGraphStage = new SimulationGraphStage(
      name = props.logLabel,
      optionalInitialCrunchMinutes = maybeCrunchMinutes,
      optionalInitialStaffMinutes = maybeStaffMinutes,
      airportConfig = props.airportConfig,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now,
      simulate = props.simulator,
      crunchPeriodStartMillis = crunchStartDateProvider,
      minutesToCrunch = props.minutesToCrunch)

    val portStateGraphStage = new PortStateGraphStage(
      name = props.logLabel,
      optionalInitialPortState = props.initialPortState,
      airportConfig = props.airportConfig,
      expireAfterMillis = props.expireAfterMillis,
      now = props.now)

    val crunchSystem = RunnableCrunch(
      props.arrivalsBaseSource, props.arrivalsFcstSource, props.arrivalsLiveSource, props.manifestsLiveSource, props.manifestsHistoricSource, shiftsSource, fixedPointsSource, staffMovementsSource, actualDesksAndQueuesSource,
      arrivalsStage, arrivalSplitsGraphStage, splitsPredictorStage, workloadGraphStage, loadBatcher, crunchLoadGraphStage, staffGraphStage, staffBatcher, simulationGraphStage, portStateGraphStage, fcstArrivalsDiffingStage, liveArrivalsDiffingStage,
      props.actors("base-arrivals").actorRef, props.actors("forecast-arrivals").actorRef, props.actors("live-arrivals").actorRef,
      props.voyageManifestsActor, props.voyageManifestsRequestActor,
      props.liveCrunchStateActor, props.forecastCrunchStateActor,
      props.actors("aggregated-arrivals").actorRef,
      crunchStartDateProvider, props.now, props.airportConfig.queues
    )

    val (baseIn, fcstIn, liveIn, manifestsLiveIn, manifestsHistoricIn, shiftsIn, fixedPointsIn, movementsIn, actDesksIn, arrivalsKillSwitch, manifestsKillSwitch) = crunchSystem.run

    CrunchSystem(
      shifts = shiftsIn,
      fixedPoints = fixedPointsIn,
      staffMovements = movementsIn,
      baseArrivalsResponse = baseIn,
      forecastArrivalsResponse = fcstIn,
      liveArrivalsResponse = liveIn,
      manifestsLiveResponse = manifestsLiveIn,
      manifestsHistoricResponse = manifestsHistoricIn,
      actualDeskStats = actDesksIn,
      List(arrivalsKillSwitch, manifestsKillSwitch)
    )
  }

  def arrivalsDiffingStage(initialArrivals: Seq[Arrival]) = new ArrivalsDiffingStage(initialArrivals)

  def initialStaffMinutesFromPortState(initialPortState: Option[PortState]): Option[StaffMinutes] = initialPortState.map(
    ps => StaffMinutes(ps.staffMinutes))

  def initialCrunchMinutesFromPortState(initialPortState: Option[PortState]): Option[CrunchMinutes] = initialPortState.map(
    ps => CrunchMinutes(ps.crunchMinutes.values.toSet))

  def initialLoadsFromPortState(initialPortState: Option[PortState]): Option[Loads] = initialPortState.map(ps => Loads(ps.crunchMinutes.values.toSeq))

  def initialFlightsFromPortState(initialPortState: Option[PortState], removeSplits: Boolean): Option[FlightsWithSplits] = initialPortState.map { ps =>
    val initialFlightsWithSplits = ps.flights.values.toSeq
    val flightsWithSplits = if (removeSplits) initialFlightsWithSplits.map { fws =>
      fws.copy(splits = fws.splits.filter(_.source == SplitSources.TerminalAverage))
    } else initialFlightsWithSplits
    
    FlightsWithSplits(flightsWithSplits, Set())
  }
}
