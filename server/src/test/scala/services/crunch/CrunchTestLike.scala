package services.crunch

import actors.Sizes.oneMegaByte
import actors._
import actors.acking.AckingReceiver.Ack
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props, Scheduler}
import akka.pattern.AskableActorRef
import akka.stream.QueueOfferResult.Enqueued
import akka.stream.scaladsl.{Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy, QueueOfferResult, UniqueKillSwitch}
import akka.testkit.{TestKit, TestProbe}
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{FlightsWithSplits, QueueName, TerminalName}
import drt.shared.PaxTypes.{B5JPlusNational, B5JPlusNationalBelowEGateAge, EeaBelowEGateAge, EeaMachineReadable, EeaNonMachineReadable, NonVisaNational, Transit, VisaNational}
import drt.shared.PaxTypesAndQueues._
import drt.shared.SplitRatiosNs.{SplitRatio, SplitRatios, SplitSources}
import drt.shared._
import graphs.SinkToSourceBridge
import manifests.passengers.BestAvailableManifest
import org.slf4j.{Logger, LoggerFactory}
import org.specs2.mutable.SpecificationLike
import server.feeds.{ArrivalsFeedResponse, ManifestsFeedResponse}
import services._
import services.crunch.deskrecs.{GetFlights, RunnableDeskRecs}
import services.graphstages.CrunchMocks
import slickdb.Tables

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}


class CrunchStateMockActor extends Actor {
  override def receive: Receive = {
    case _ => sender() ! Ack
  }
}

class PortStateTestActor(liveActor: ActorRef, forecastActor: ActorRef, airportConfig: AirportConfig, probe: ActorRef, expireAfterMillis: Long, now: () => SDateLike, liveDaysAhead: Int)
  extends PortStateActor(liveActor, forecastActor, airportConfig, expireAfterMillis, now, liveDaysAhead) {

  val state = new PortStateMutable

  override def updateActorForDay(day: String, updates: PortStateMinutes): Future[Any] = {
    log.info(s"Received updates for our test state")
    updates.applyTo(state, now().millisSinceEpoch)
    probe ! state.immutable
    Future()
  }

  def testReceive: Receive = {
    case initialPortState: PortState =>
      state.flights ++= initialPortState.flights
      state.crunchMinutes ++= initialPortState.crunchMinutes
      state.staffMinutes ++= initialPortState.staffMinutes
  }

  override def receive: Receive = testReceive orElse super.receive

  override def makeRequest[X](startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch, request: Any, reduce: Iterable[X] => X): Unit = request match {
    case GetPortState(startMillis, endMillis) =>
      log.info(s"Received GetPortState(${SDate(startMillis).toISOString()}, ${SDate(endMillis).toISOString()}) request")
      sender() ! Option(state.window(SDate(startMillis), SDate(endMillis)))

    case GetPortStateForTerminal(startMillis, endMillis, terminalName) =>
      log.info(s"Received GetPortState(${SDate(startMillis).toISOString()}, ${SDate(endMillis).toISOString()}, $terminalName) request")
      sender() ! Option(state.windowWithTerminalFilter(SDate(startMillis), SDate(endMillis), Seq(terminalName)))

    case GetUpdatesSince(sinceMillis, startMillis, endMillis) =>
      log.info(s"Received GetUpdatesSince(${SDate(startMillis).toISOString()}, ${SDate(endMillis).toISOString()}) request")
      sender() ! state.updates(sinceMillis, startMillis, endMillis)

    case GetFlights(startMillis, endMillis) =>
      log.info(s"Received GetFlights(${SDate(startMillis).toISOString()}, ${SDate(endMillis).toISOString()})")
      val flightsToSend = state.flights.range(SDate(startMillis), SDate(endMillis)).values.toList
      sender() ! FlightsWithSplits(flightsToSend, List())
  }
}

object PortStateTestActor {
  def props(liveActor: ActorRef, forecastActor: ActorRef, airportConfig: AirportConfig, probe: ActorRef, expireAfterMillis: Long, now: () => SDateLike, liveDaysAhead: Int): Props =
    Props(new PortStateTestActor(liveActor, forecastActor, airportConfig, probe, expireAfterMillis, now, liveDaysAhead))
}

case class CrunchGraphInputsAndProbes(baseArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      forecastArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      liveArrivalsInput: SourceQueueWithComplete[ArrivalsFeedResponse],
                                      manifestsLiveInput: SourceQueueWithComplete[ManifestsFeedResponse],
                                      shiftsInput: SourceQueueWithComplete[ShiftAssignments],
                                      fixedPointsInput: SourceQueueWithComplete[FixedPointAssignments],
                                      liveStaffMovementsInput: SourceQueueWithComplete[Seq[StaffMovement]],
                                      forecastStaffMovementsInput: SourceQueueWithComplete[Seq[StaffMovement]],
                                      actualDesksAndQueuesInput: SourceQueueWithComplete[ActualDeskStats],
                                      portStateTestProbe: TestProbe,
                                      baseArrivalsTestProbe: TestProbe,
                                      forecastArrivalsTestProbe: TestProbe,
                                      liveArrivalsTestProbe: TestProbe,
                                      aggregatedArrivalsActor: ActorRef,
                                      portStateActor: ActorRef)


object H2Tables extends {
  val profile = slick.jdbc.H2Profile
} with Tables

class CrunchTestLike
  extends TestKit(ActorSystem("StreamingCrunchTests"))
    with SpecificationLike {
  isolated
  sequential

  implicit val actorSystem: ActorSystem = system
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val scheduler: Scheduler = system.scheduler
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  val log: Logger = LoggerFactory.getLogger(getClass)

  val crunchStateMockActor: ActorRef = system.actorOf(Props(classOf[CrunchStateMockActor]), "crunch-state-mock")

  val oneMinuteMillis = 60000
  val uniquifyArrivals: Seq[ApiFlightWithSplits] => List[(ApiFlightWithSplits, Set[Arrival])] =
    CodeShares.uniqueArrivalsWithCodeShares((f: ApiFlightWithSplits) => f.apiFlight)

  val airportConfig = AirportConfig(
    portCode = "STN",
    queues = Map("T1" -> Seq(Queues.EeaDesk, Queues.NonEeaDesk), "T2" -> Seq(Queues.EeaDesk, Queues.NonEeaDesk)),
    slaByQueue = Map(Queues.EeaDesk -> 25, Queues.EGate -> 20, Queues.NonEeaDesk -> 45),
    terminalNames = Seq("T1", "T2"),
    defaultWalkTimeMillis = Map(),
    terminalPaxSplits = List("T1", "T2").map(t => (t, SplitRatios(
      SplitSources.TerminalAverage,
      SplitRatio(eeaMachineReadableToDesk, 1)
    ))).toMap,
    terminalProcessingTimes = Map(
      "T1" -> Map(
        eeaMachineReadableToDesk -> 25d / 60,
        eeaNonMachineReadableToDesk -> 25d / 60
      ),
      "T2" -> Map(
        eeaMachineReadableToDesk -> 25d / 60,
        eeaNonMachineReadableToDesk -> 25d / 60
      )
    ),
    minMaxDesksByTerminalQueue = Map(
      "T1" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.NonEeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20)))),
      "T2" -> Map(
        Queues.EeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.NonEeaDesk -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))),
        Queues.EGate -> ((List.fill[Int](24)(1), List.fill[Int](24)(20))))),
    timeToChoxMillis = 120000L,
    firstPaxOffMillis = 180000L,
    role = STNAccess,
    terminalPaxTypeQueueAllocation = Map(
      "T1" -> Map(
        EeaMachineReadable -> List(Queues.EGate -> 0.8, Queues.EeaDesk -> 0.2),
        EeaBelowEGateAge -> List(Queues.EeaDesk -> 1.0),
        EeaNonMachineReadable -> List(Queues.EeaDesk -> 1.0),
        NonVisaNational -> List(Queues.NonEeaDesk -> 1.0),
        VisaNational -> List(Queues.NonEeaDesk -> 1.0),
        B5JPlusNational -> List(Queues.EGate -> 0.6, Queues.EeaDesk -> 0.4),
        B5JPlusNationalBelowEGateAge -> List(Queues.EeaDesk -> 1)
      ),
      "T2" -> Map(
        EeaMachineReadable -> List(Queues.EeaDesk -> 1),
        EeaBelowEGateAge -> List(Queues.EeaDesk -> 1.0),
        EeaNonMachineReadable -> List(Queues.EeaDesk -> 1.0),
        NonVisaNational -> List(Queues.NonEeaDesk -> 1.0),
        VisaNational -> List(Queues.NonEeaDesk -> 1.0),
        B5JPlusNational -> List(Queues.EeaDesk -> 1),
        B5JPlusNationalBelowEGateAge -> List(Queues.EeaDesk -> 1)
      )
    )
  )

  val pcpForFlightFromSch: Arrival => MilliDate = (a: Arrival) => MilliDate(SDate(a.Scheduled).millisSinceEpoch)
  val pcpForFlightFromBest: Arrival => MilliDate = (a: Arrival) => {
    if (a.ActualChox.isDefined) MilliDate(SDate(a.ActualChox.get).millisSinceEpoch)
    else if (a.EstimatedChox.isDefined) MilliDate(SDate(a.EstimatedChox.get).millisSinceEpoch)
    else if (a.Actual.isDefined) MilliDate(SDate(a.Actual.get).millisSinceEpoch)
    else if (a.Estimated.isDefined) MilliDate(SDate(a.Estimated.get).millisSinceEpoch)
    else MilliDate(SDate(a.Scheduled).millisSinceEpoch)
  }

  def createPortStateActor(name: String = "", testProbe: TestProbe, now: () => SDateLike): ActorRef = {
    system.actorOf(PortStateTestActor.props(crunchStateMockActor, crunchStateMockActor, airportConfig, testProbe.ref, 24 * 360000L, now, 100), name = "port-state-actor")
  }

  def testProbe(name: String) = TestProbe(name = name)

  def runCrunchGraph(initialForecastBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap(),
                     initialForecastArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap(),
                     initialLiveBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap(),
                     initialLiveArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap(),
                     initialPortState: Option[PortState] = None,
                     airportConfig: AirportConfig = airportConfig,
                     csvSplitsProvider: SplitsProvider.SplitProvider = (_, _) => None,
                     pcpArrivalTime: Arrival => MilliDate = pcpForFlightFromSch,
                     minutesToCrunch: Int = 60,
                     expireAfterMillis: Long = DrtStaticParameters.expireAfterMillis,
                     calcPcpWindow: (Set[ApiFlightWithSplits], Set[ApiFlightWithSplits]) => Option[(SDateLike, SDateLike)] = (_, _) => Some((SDate.now(), SDate.now())),
                     now: () => SDateLike,
                     initialShifts: ShiftAssignments = ShiftAssignments.empty,
                     initialFixedPoints: FixedPointAssignments = FixedPointAssignments.empty,
                     initialStaffMovements: Seq[StaffMovement] = Seq(),
                     logLabel: String = "",
                     cruncher: TryCrunch = CrunchMocks.mockCrunch,
                     simulator: Simulator = CrunchMocks.mockSimulator,
                     aggregatedArrivalsActor: ActorRef = testProbe("aggregated-arrivals").ref,
                     useLegacyManifests: Boolean = false,
                     maxDaysToCrunch: Int = 2,
                     checkRequiredStaffUpdatesOnStartup: Boolean = false
                    ): CrunchGraphInputsAndProbes = {

    val portStateProbe = testProbe("portstate")
    val forecastBaseArrivalsProbe = testProbe("forecast-base-arrivals")
    val forecastArrivalsProbe = testProbe("forecast-arrivals")
    val liveBaseArrivalsProbe = testProbe("live-base-arrivals")
    val liveArrivalsProbe = testProbe("live-arrivals")

    val shiftsActor: ActorRef = system.actorOf(Props(classOf[ShiftsActor], now, DrtStaticParameters.timeBeforeThisMonth(now)))
    val fixedPointsActor: ActorRef = system.actorOf(Props(classOf[FixedPointsActor], now))
    val staffMovementsActor: ActorRef = system.actorOf(Props(classOf[StaffMovementsActor], now, DrtStaticParameters.time48HoursAgo(now)))
    val snapshotInterval = 1
    val manifestsActor: ActorRef = system.actorOf(Props(classOf[VoyageManifestsActor], oneMegaByte, now, DrtStaticParameters.expireAfterMillis, Option(snapshotInterval)))

    val portStateActor = createPortStateActor(logLabel, portStateProbe, now)
    initialPortState.foreach(ps => portStateActor ! ps)

    val retrier = Retry.retry[FlightsWithSplits](RetryDelays.fibonacci, 5, 5 seconds) _

    val (millisToCrunchActor: ActorRef, _: UniqueKillSwitch) = RunnableDeskRecs(portStateActor, minutesToCrunch, cruncher, airportConfig, retrier).run()
    portStateActor ! SetCrunchActor(millisToCrunchActor)

    val manifestsSource: Source[ManifestsFeedResponse, SourceQueueWithComplete[ManifestsFeedResponse]] = Source.queue[ManifestsFeedResponse](0, OverflowStrategy.backpressure)
    val liveArrivals: Source[ArrivalsFeedResponse, SourceQueueWithComplete[ArrivalsFeedResponse]] = Source.queue[ArrivalsFeedResponse](0, OverflowStrategy.backpressure)
    val liveBaseArrivals: Source[ArrivalsFeedResponse, SourceQueueWithComplete[ArrivalsFeedResponse]] = Source.queue[ArrivalsFeedResponse](0, OverflowStrategy.backpressure)
    val forecastArrivals: Source[ArrivalsFeedResponse, SourceQueueWithComplete[ArrivalsFeedResponse]] = Source.queue[ArrivalsFeedResponse](0, OverflowStrategy.backpressure)
    val forecastBaseArrivals: Source[ArrivalsFeedResponse, SourceQueueWithComplete[ArrivalsFeedResponse]] = Source.queue[ArrivalsFeedResponse](0, OverflowStrategy.backpressure)

    val (_, _, manifestRequestsSink) = SinkToSourceBridge[List[Arrival]]
    val (manifestResponsesSource, _, _) = SinkToSourceBridge[List[BestAvailableManifest]]

    val crunchInputs = CrunchSystem(CrunchProps(
      logLabel = logLabel,
      airportConfig = airportConfig,
      pcpArrival = pcpArrivalTime,
      historicalSplitsProvider = csvSplitsProvider,
      portStateActor = portStateActor,
      maxDaysToCrunch = maxDaysToCrunch,
      expireAfterMillis = expireAfterMillis,
      minutesToCrunch = minutesToCrunch,
      actors = Map[String, AskableActorRef](
        "shifts" -> shiftsActor,
        "fixed-points" -> fixedPointsActor,
        "staff-movements" -> staffMovementsActor,
        "forecast-base-arrivals" -> forecastBaseArrivalsProbe.ref,
        "forecast-arrivals" -> forecastArrivalsProbe.ref,
        "live-base-arrivals" -> liveBaseArrivalsProbe.ref,
        "live-arrivals" -> liveArrivalsProbe.ref,
        "aggregated-arrivals" -> aggregatedArrivalsActor
      ),
      useNationalityBasedProcessingTimes = false,
      useLegacyManifests = useLegacyManifests,
      now = now,
      b5JStartDate = SDate("2019-06-01"),
      manifestsLiveSource = manifestsSource,
      manifestResponsesSource = manifestResponsesSource,
      voyageManifestsActor = manifestsActor,
      manifestRequestsSink = manifestRequestsSink,
      simulator = simulator,
      initialPortState = initialPortState,
      initialForecastBaseArrivals = initialForecastBaseArrivals,
      initialForecastArrivals = initialForecastArrivals,
      initialLiveBaseArrivals = initialLiveBaseArrivals,
      initialLiveArrivals = initialLiveArrivals,
      arrivalsForecastBaseSource = forecastBaseArrivals,
      arrivalsForecastSource = forecastArrivals,
      arrivalsLiveBaseSource = liveBaseArrivals,
      arrivalsLiveSource = liveArrivals,
      initialShifts = initialShifts,
      initialFixedPoints = initialFixedPoints,
      initialStaffMovements = initialStaffMovements,
      checkRequiredStaffUpdatesOnStartup = checkRequiredStaffUpdatesOnStartup,
      stageThrottlePer = 50 milliseconds
    ))

    portStateActor ! SetSimulationActor(crunchInputs.loadsToSimulate)

    CrunchGraphInputsAndProbes(
      baseArrivalsInput = crunchInputs.forecastBaseArrivalsResponse,
      forecastArrivalsInput = crunchInputs.forecastArrivalsResponse,
      liveArrivalsInput = crunchInputs.liveArrivalsResponse,
      manifestsLiveInput = crunchInputs.manifestsLiveResponse,
      shiftsInput = crunchInputs.shifts,
      fixedPointsInput = crunchInputs.fixedPoints,
      liveStaffMovementsInput = crunchInputs.staffMovements,
      forecastStaffMovementsInput = crunchInputs.staffMovements,
      actualDesksAndQueuesInput = crunchInputs.actualDeskStats,
      portStateTestProbe = portStateProbe,
      baseArrivalsTestProbe = forecastBaseArrivalsProbe,
      forecastArrivalsTestProbe = forecastArrivalsProbe,
      liveArrivalsTestProbe = liveArrivalsProbe,
      aggregatedArrivalsActor = aggregatedArrivalsActor,
      portStateActor = portStateActor
    )
  }

  def paxLoadsFromPortState(portState: PortState, minsToTake: Int, startFromMinuteIdx: Int = 0): Map[TerminalName, Map[QueueName, List[Double]]] = portState
    .crunchMinutes
    .values
    .groupBy(_.terminalName)
    .map {
      case (tn, tms) =>
        val terminalLoads = tms
          .groupBy(_.queueName)
          .map {
            case (qn, qms) =>
              val paxLoad = qms
                .toList
                .sortBy(_.minute)
                .map(_.paxLoad)
                .slice(startFromMinuteIdx, startFromMinuteIdx + minsToTake)
              (qn, paxLoad)
          }
        (tn, terminalLoads)
    }

  def paxLoadsFromPortState(portState: PortState, minsToTake: Int, startFromMinute: SDateLike): Map[TerminalName, Map[QueueName, List[Double]]] = {
    val startFromMillis = startFromMinute.millisSinceEpoch

    portState
      .crunchMinutes
      .values
      .groupBy(_.terminalName)
      .map {
        case (tn, tms) =>
          val terminalLoads = tms
            .groupBy(_.queueName)
            .map {
              case (qn, qms) =>
                val startIdx = qms
                  .toList
                  .sortBy(_.minute)
                  .indexWhere(_.minute == startFromMillis)
                val paxLoad = qms
                  .toList
                  .sortBy(_.minute)
                  .map(_.paxLoad)
                  .slice(startIdx, startIdx + minsToTake)
                (qn, paxLoad)
            }
          (tn, terminalLoads)
      }
  }

  def allWorkLoadsFromPortState(portState: PortState): Map[TerminalName, Map[QueueName, List[Double]]] = portState
    .crunchMinutes
    .values
    .groupBy(_.terminalName)
    .map {
      case (tn, tms) =>
        val terminalLoads = tms
          .groupBy(_.queueName)
          .map {
            case (qn, qms) =>
              val sortedCms = qms.toList.sortBy(_.minute)
              val workLoad = sortedCms.map(_.workLoad)
              (qn, workLoad)
          }
        (tn, terminalLoads)
    }

  def workLoadsFromPortState(portState: PortState, minsToTake: Int): Map[TerminalName, Map[QueueName, List[Double]]] = portState
    .crunchMinutes
    .values
    .groupBy(_.terminalName)
    .map {
      case (tn, tms) =>
        val terminalLoads = tms
          .groupBy(_.queueName)
          .map {
            case (qn, qms) =>
              val sortedCms = qms.toList.sortBy(_.minute)
              val workLoad = sortedCms.map(_.workLoad).take(minsToTake)
              (qn, workLoad)
          }
        (tn, terminalLoads)
    }

  def deskRecsFromPortState(portState: PortState, minsToTake: Int): Map[TerminalName, Map[QueueName, List[Int]]] = portState
    .crunchMinutes
    .values
    .groupBy(_.terminalName)
    .map {
      case (tn, tms) =>
        val terminalLoads = tms
          .groupBy(_.queueName)
          .map {
            case (qn, qms) =>
              val sortedCms = qms.toList.sortBy(_.minute)
              val deskRecs = sortedCms.map(_.deskRec).take(minsToTake)
              (qn, deskRecs)
          }
        (tn, terminalLoads)
    }

  def offerAndWait[T](sourceQueue: SourceQueueWithComplete[T], offering: T): QueueOfferResult = {
    Await.result(sourceQueue.offer(offering), 3 seconds) match {
      case offerResult if offerResult != Enqueued =>
        throw new Exception(s"Queue offering (${offering.getClass}) was not enqueued: ${offerResult.getClass}")
      case offerResult =>
        offerResult
    }
  }
}
