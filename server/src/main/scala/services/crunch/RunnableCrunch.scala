package services.crunch

import akka.actor.ActorRef
import akka.stream._
import akka.stream.scaladsl.{Broadcast, GraphDSL, RunnableGraph, Sink, Source}
import akka.stream.stage.GraphStage
import drt.chroma.ArrivalsDiffingStage
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{FlightsWithSplits, QueueName, TerminalName}
import drt.shared._
import manifests.passengers.BestAvailableManifest
import org.slf4j.{Logger, LoggerFactory}
import drt.server.feeds._
import services.graphstages.Crunch.Loads
import services.graphstages._

object RunnableCrunch {
  val log: Logger = LoggerFactory.getLogger(getClass)

  val oneDayMillis: Int = 60 * 60 * 24 * 1000

  def groupByCodeShares(flights: Seq[ApiFlightWithSplits]): Seq[(ApiFlightWithSplits, Set[Arrival])] = flights.map(f => (f, Set(f.apiFlight)))

  def apply[FR, MS, SS, SFP, SMM, SAD](baseArrivalsSource: Source[ArrivalsFeedResponse, FR],
                                       fcstArrivalsSource: Source[ArrivalsFeedResponse, FR],
                                       liveArrivalsSource: Source[ArrivalsFeedResponse, FR],
                                       manifestsLiveSource: Source[ManifestsFeedResponse, MS],
                                       manifestsHistoricSource: Source[ManifestsFeedResponse, MS],
                                       shiftsSource: Source[ShiftAssignments, SS],
                                       fixedPointsSource: Source[FixedPointAssignments, SFP],
                                       staffMovementsSource: Source[Seq[StaffMovement], SMM],
                                       actualDesksAndWaitTimesSource: Source[ActualDeskStats, SAD],

                                       arrivalsGraphStage: ArrivalsGraphStage,
                                       arrivalSplitsStage: GraphStage[FanInShape3[ArrivalsDiff, ManifestsFeedResponse, ManifestsFeedResponse, FlightsWithSplits]],
                                       splitsPredictorStage: SplitsPredictorBase,
                                       workloadGraphStage: WorkloadGraphStage,
                                       loadBatchUpdateGraphStage: BatchLoadsByCrunchPeriodGraphStage,
                                       crunchLoadGraphStage: CrunchLoadGraphStage,
                                       staffGraphStage: StaffGraphStage,
                                       staffBatchUpdateGraphStage: StaffBatchUpdateGraphStage,
                                       simulationGraphStage: SimulationGraphStage,
                                       portStateGraphStage: PortStateGraphStage,

                                       fcstArrivalsDiffStage: ArrivalsDiffingStage,
                                       liveArrivalsDiffStage: ArrivalsDiffingStage,

                                       baseArrivalsActor: ActorRef,
                                       fcstArrivalsActor: ActorRef,
                                       liveArrivalsActor: ActorRef,

                                       manifestsActor: ActorRef,
                                       manifestsRequestActor: ActorRef,

                                       liveCrunchStateActor: ActorRef,
                                       fcstCrunchStateActor: ActorRef,
                                       aggregatedArrivalsStateActor: ActorRef,

                                       queueLoadActor: ActorRef,

                                       crunchPeriodStartMillis: SDateLike => SDateLike,
                                       now: () => SDateLike,
                                       portQueues: Map[TerminalName, Seq[QueueName]]
                                      ): RunnableGraph[(FR, FR, FR, MS, MS, SS, SFP, SMM, SAD, UniqueKillSwitch, UniqueKillSwitch)] = {

    val arrivalsKillSwitch = KillSwitches.single[ArrivalsDiff]

    val manifestsKillSwitch = KillSwitches.single[ManifestsFeedResponse]


    import akka.stream.scaladsl.GraphDSL.Implicits._

    val graph = GraphDSL.create(
      baseArrivalsSource.async,
      fcstArrivalsSource.async,
      liveArrivalsSource.async,
      manifestsLiveSource.async,
      manifestsHistoricSource.async,
      shiftsSource.async,
      fixedPointsSource.async,
      staffMovementsSource.async,
      actualDesksAndWaitTimesSource.async,
      arrivalsKillSwitch,
      manifestsKillSwitch
    )((_, _, _, _, _, _, _, _, _, _, _)) {

      implicit builder =>
        (
          baseArrivals,
          fcstArrivals,
          liveArrivals,
          manifestsLive,
          manifestsHistoric,
          shifts,
          fixedPoints,
          staffMovements,
          actualDesksAndWaitTimes,
          arrivalsGraphKillSwitch,
          manifestGraphKillSwitch
        ) =>
          val arrivals = builder.add(arrivalsGraphStage.async)
          val arrivalSplits = builder.add(arrivalSplitsStage.async)
          val workload = builder.add(workloadGraphStage.async)
          val batchLoad = builder.add(loadBatchUpdateGraphStage.async)
          val crunch = builder.add(crunchLoadGraphStage.async)
          val staff = builder.add(staffGraphStage.async)
          val batchStaff = builder.add(staffBatchUpdateGraphStage.async)
          val simulation = builder.add(simulationGraphStage.async)
          val portState = builder.add(portStateGraphStage.async)
          val fcstArrivalsDiffing = builder.add(fcstArrivalsDiffStage.async)
          val liveArrivalsDiffing = builder.add(liveArrivalsDiffStage.async)

          val baseArrivalsFanOut = builder.add(Broadcast[ArrivalsFeedResponse](2))
          val fcstArrivalsFanOut = builder.add(Broadcast[ArrivalsFeedResponse](2))
          val liveArrivalsFanOut = builder.add(Broadcast[ArrivalsFeedResponse](2))

          val arrivalsFanOut = builder.add(Broadcast[ArrivalsDiff](2))

          val manifestsFanOut = builder.add(Broadcast[ManifestsFeedResponse](2))
          val arrivalSplitsFanOut = builder.add(Broadcast[FlightsWithSplits](2))
          val workloadFanOut = builder.add(Broadcast[Loads](2))
          val batchedWorkloadFanOut = builder.add(Broadcast[Loads](2))
          val staffFanOut = builder.add(Broadcast[StaffMinutes](2))
          val portStateFanOut = builder.add(Broadcast[PortStateWithDiff](3))

          val baseArrivalsSink = builder.add(Sink.actorRef(baseArrivalsActor, "complete"))
          val fcstArrivalsSink = builder.add(Sink.actorRef(fcstArrivalsActor, "complete"))
          val liveArrivalsSink = builder.add(Sink.actorRef(liveArrivalsActor, "complete"))

          val manifestsSink = builder.add(Sink.actorRef(manifestsActor, "complete"))

          val liveSink = builder.add(Sink.actorRef(liveCrunchStateActor, "complete"))
          val fcstSink = builder.add(Sink.actorRef(fcstCrunchStateActor, "complete"))
          val aggregatedArrivalsSink = builder.add(Sink.actorRef(aggregatedArrivalsStateActor, "complete"))
          val manifestsRequestSink = builder.add(Sink.actorRef(manifestsRequestActor, "complete"))
          val queueLoadSink = builder.add(Sink.actorRef(queueLoadActor, "complete"))

          // @formatter:off
          baseArrivals ~> baseArrivalsFanOut ~> arrivals.in0
                          baseArrivalsFanOut ~> baseArrivalsSink

          fcstArrivals ~> fcstArrivalsDiffing ~> fcstArrivalsFanOut ~> arrivals.in1
                                                 fcstArrivalsFanOut ~> fcstArrivalsSink

          liveArrivals ~> liveArrivalsDiffing ~> liveArrivalsFanOut ~> arrivals.in2
                                                 liveArrivalsFanOut ~> liveArrivalsSink

          manifestsLive ~> manifestsFanOut

          manifestsFanOut.out(0).conflate[ManifestsFeedResponse] {
              case (bm, ManifestsFeedFailure(_, _)) => bm
              case (ManifestsFeedSuccess(DqManifests(_, acc), _), ManifestsFeedSuccess(DqManifests(_, ms), createdAt)) =>
                val existingManifests = acc.toSeq.map(vm => BestAvailableManifest(vm))
                val newManifests = ms.toSeq.map(vm => BestAvailableManifest(vm))
                BestManifestsFeedSuccess(existingManifests ++ newManifests, createdAt)
              case (BestManifestsFeedSuccess(acc, _), ManifestsFeedSuccess(DqManifests(_, ms), createdAt)) =>
                val newManifests = ms.toSeq.map(vm => BestAvailableManifest(vm))
                BestManifestsFeedSuccess(acc ++ newManifests, createdAt)
            } ~> manifestGraphKillSwitch ~> arrivalSplits.in1

          manifestsFanOut.out(1) ~> manifestsSink

          manifestsHistoric.out.conflate[ManifestsFeedResponse] {
            case (BestManifestsFeedSuccess(acc, _), BestManifestsFeedSuccess(newManifests, createdAt)) =>
              BestManifestsFeedSuccess(acc ++ newManifests, createdAt)
          } ~> arrivalSplits.in2


          shifts          ~> staff.in0
          fixedPoints     ~> staff.in1
          staffMovements  ~> staff.in2

          arrivals.out ~> arrivalsGraphKillSwitch ~> arrivalsFanOut ~> arrivalSplits.in0
                                                     arrivalsFanOut ~> manifestsRequestSink

          arrivalSplits.out ~> arrivalSplitsFanOut ~> workload
                               arrivalSplitsFanOut ~> portState.in0

          workload.out ~> workloadFanOut ~> batchLoad ~> batchedWorkloadFanOut ~> crunch
                                                         batchedWorkloadFanOut ~> simulation.in0
                          workloadFanOut ~> queueLoadSink

          crunch                   ~> portState.in1
          actualDesksAndWaitTimes  ~> portState.in2
          staff.out ~> staffFanOut ~> portState.in3
                       staffFanOut ~> batchStaff ~> simulation.in1

          simulation.out ~> portState.in4

          portState.out ~> portStateFanOut
                           portStateFanOut.map(_.window(liveStart(now), liveEnd(now), portQueues))                ~> liveSink
                           portStateFanOut.map(_.window(forecastStart(now), forecastEnd(now), portQueues))        ~> fcstSink
                           portStateFanOut.map(pswd => withOnlyDescheduledRemovals(pswd.diff, now())) ~> aggregatedArrivalsSink
          // @formatter:on

          ClosedShape
    }

    RunnableGraph.fromGraph(graph)
  }

  def withOnlyDescheduledRemovals: (PortStateDiff, SDateLike) => PortStateDiff = (diff: PortStateDiff, now: SDateLike) => {
    val nowMillis = now.millisSinceEpoch
    diff.copy(flightRemovals = diff.flightRemovals.filterNot(_.flightKey.scheduled <= nowMillis))
  }

  def liveStart(now: () => SDateLike): SDateLike = Crunch.getLocalLastMidnight(now()).addDays(-1)

  def liveEnd(now: () => SDateLike): SDateLike = Crunch.getLocalNextMidnight(now()).addDays(1)

  def forecastStart(now: () => SDateLike): SDateLike = Crunch.getLocalNextMidnight(now())

  def forecastEnd(now: () => SDateLike): SDateLike = Crunch.getLocalNextMidnight(now()).addDays(180)
}
