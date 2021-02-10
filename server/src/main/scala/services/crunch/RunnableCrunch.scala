package services.crunch

import actors.acking.AckingReceiver._
import actors.queues.QueueLikeActor.UpdatedMillis
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.stream._
import akka.stream.scaladsl.{Broadcast, GraphDSL, RunnableGraph, Sink, Source}
import akka.stream.stage.GraphStage
import drt.chroma.ArrivalsDiffingStage
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{Flights, FlightsWithSplitsDiff}
import drt.shared.api.Arrival
import drt.shared._
import manifests.passengers.BestAvailableManifest
import org.slf4j.{Logger, LoggerFactory}
import server.feeds._
import services.StreamSupervision
import services.graphstages._

import scala.concurrent.Future
import scala.concurrent.duration._

object RunnableCrunch {
  val log: Logger = LoggerFactory.getLogger(getClass)

  val oneDayMillis: Int = 60 * 60 * 24 * 1000

  def groupByCodeShares(flights: Seq[ApiFlightWithSplits]): Seq[(ApiFlightWithSplits, Set[Arrival])] = flights.map(f => (f, Set(f.apiFlight)))

  def apply[FR, MS, SS, SFP, SMM, SAD](forecastBaseArrivalsSource: Source[ArrivalsFeedResponse, FR],
                                       forecastArrivalsSource: Source[ArrivalsFeedResponse, FR],
                                       liveBaseArrivalsSource: Source[ArrivalsFeedResponse, FR],
                                       liveArrivalsSource: Source[ArrivalsFeedResponse, FR],
                                       manifestsLiveSource: Source[ManifestsFeedResponse, MS],
                                       manifestResponsesSource: Source[List[BestAvailableManifest], NotUsed],
                                       shiftsSource: Source[ShiftAssignments, SS],
                                       fixedPointsSource: Source[FixedPointAssignments, SFP],
                                       staffMovementsSource: Source[Seq[StaffMovement], SMM],
                                       actualDesksAndWaitTimesSource: Source[ActualDeskStats, SAD],

                                       arrivalsGraphStage: ArrivalsGraphStage,
                                       arrivalSplitsStage: GraphStage[FanInShape3[ArrivalsDiff, List[BestAvailableManifest], List[BestAvailableManifest], FlightsWithSplitsDiff]],
                                       staffGraphStage: StaffGraphStage,

                                       forecastArrivalsDiffStage: ArrivalsDiffingStage,
                                       liveBaseArrivalsDiffStage: ArrivalsDiffingStage,
                                       liveArrivalsDiffStage: ArrivalsDiffingStage,

                                       forecastBaseArrivalsActor: ActorRef,
                                       forecastArrivalsActor: ActorRef,
                                       liveBaseArrivalsActor: ActorRef,
                                       liveArrivalsActor: ActorRef,
                                       applyPaxDeltas: List[Arrival] => Future[List[Arrival]],

                                       manifestsActor: ActorRef,
                                       manifestRequestsSink: Sink[List[Arrival], NotUsed],

                                       portStateActor: ActorRef,
                                       aggregatedArrivalsStateActor: ActorRef,
                                       deploymentRequestActor: ActorRef,

                                       forecastMaxMillis: () => MillisSinceEpoch,
                                       throttleDurationPer: FiniteDuration
                                      )
                                      (implicit mat: Materializer, system: ActorSystem): RunnableGraph[(FR, FR, FR, FR, MS, SS, SFP, SMM, SAD, UniqueKillSwitch, UniqueKillSwitch, UniqueKillSwitch, UniqueKillSwitch, UniqueKillSwitch)] = {

    val arrivalsKillSwitch = KillSwitches.single[ArrivalsFeedResponse]
    val manifestsLiveKillSwitch = KillSwitches.single[ManifestsFeedResponse]
    val shiftsKillSwitch = KillSwitches.single[ShiftAssignments]
    val fixedPointsKillSwitch = KillSwitches.single[FixedPointAssignments]
    val movementsKillSwitch = KillSwitches.single[Seq[StaffMovement]]

    import akka.stream.scaladsl.GraphDSL.Implicits._

    val graph = GraphDSL.create(
      forecastBaseArrivalsSource,
      forecastArrivalsSource,
      liveBaseArrivalsSource,
      liveArrivalsSource,
      manifestsLiveSource,
      shiftsSource,
      fixedPointsSource,
      staffMovementsSource,
      actualDesksAndWaitTimesSource,
      arrivalsKillSwitch,
      manifestsLiveKillSwitch,
      shiftsKillSwitch,
      fixedPointsKillSwitch,
      movementsKillSwitch
      )((_, _, _, _, _, _, _, _, _, _, _, _, _, _)) {

      implicit builder =>
        (
          forecastBaseArrivalsSourceSync,
          forecastArrivalsSourceSync,
          liveBaseArrivalsSourceSync,
          liveArrivalsSourceSync,
          manifestsLiveSourceSync,
          shiftsSourceAsync,
          fixedPointsSourceAsync,
          staffMovementsSourceAsync,
          actualDesksAndWaitTimesSourceSync,
          arrivalsKillSwitchSync,
          manifestsLiveKillSwitchSync,
          shiftsKillSwitchSync,
          fixedPointsKillSwitchSync,
          movementsKillSwitchSync
        ) =>
          def newPortStateSink(): SinkShape[Any] = {
            builder.add(Sink.actorRefWithAck(portStateActor, StreamInitialized, Ack, StreamCompleted, StreamFailure).async)
          }

          val arrivals = builder.add(arrivalsGraphStage)
          val arrivalSplits = builder.add(arrivalSplitsStage)
          val staff = builder.add(staffGraphStage)
          val deploymentRequestSink = builder.add(Sink.actorRef(deploymentRequestActor, StreamCompleted))
          val deskStatsSink = newPortStateSink()
          val flightsWithSplitsSink = newPortStateSink()

          val staffSink = newPortStateSink()
          val fcstArrivalsDiffing = builder.add(forecastArrivalsDiffStage)
          val liveBaseArrivalsDiffing = builder.add(liveBaseArrivalsDiffStage)
          val liveArrivalsDiffing = builder.add(liveArrivalsDiffStage)

          val forecastBaseArrivalsFanOut = builder.add(Broadcast[ArrivalsFeedResponse](2))
          val forecastArrivalsFanOut = builder.add(Broadcast[ArrivalsFeedResponse](2))
          val liveBaseArrivalsFanOut = builder.add(Broadcast[ArrivalsFeedResponse](2))
          val liveArrivalsFanOut = builder.add(Broadcast[ArrivalsFeedResponse](2))

          val arrivalsFanOut = builder.add(Broadcast[ArrivalsDiff](2))

          val manifestsFanOut = builder.add(Broadcast[ManifestsFeedResponse](2))
          val arrivalSplitsFanOut = builder.add(Broadcast[FlightsWithSplitsDiff](3))
          val staffFanOut = builder.add(Broadcast[StaffMinutes](2))

          val baseArrivalsSink = builder.add(Sink.actorRef(forecastBaseArrivalsActor, StreamCompleted))
          val fcstArrivalsSink = builder.add(Sink.actorRef(forecastArrivalsActor, StreamCompleted))
          val liveBaseArrivalsSink = builder.add(Sink.actorRef(liveBaseArrivalsActor, StreamCompleted))
          val liveArrivalsSink = builder.add(Sink.actorRef(liveArrivalsActor, StreamCompleted))

          val manifestsSink = builder.add(Sink.actorRef(manifestsActor, StreamCompleted))

          val arrivalUpdatesSink = builder.add(Sink.actorRefWithAck(aggregatedArrivalsStateActor, StreamInitialized, Ack, StreamCompleted, StreamFailure))
          val arrivalRemovalsSink = builder.add(Sink.actorRefWithAck(aggregatedArrivalsStateActor, StreamInitialized, Ack, StreamCompleted, StreamFailure))

          // @formatter:off
          forecastBaseArrivalsSourceSync.out.map {
            case ArrivalsFeedSuccess(Flights(as), ca) =>
              val maxScheduledMillis = forecastMaxMillis()
              ArrivalsFeedSuccess(Flights(as.filter(_.Scheduled < maxScheduledMillis)), ca)
            case failure => failure
          } ~> forecastBaseArrivalsFanOut

          forecastBaseArrivalsFanOut
            .collect { case ArrivalsFeedSuccess(Flights(as), _) => as.toList }
            .mapAsync(1)(applyPaxDeltas) ~> arrivals.in0
          forecastBaseArrivalsFanOut ~> baseArrivalsSink

          forecastArrivalsSourceSync ~> fcstArrivalsDiffing ~> forecastArrivalsFanOut

          forecastArrivalsFanOut.collect { case ArrivalsFeedSuccess(Flights(as), _) if as.nonEmpty => as.toList } ~> arrivals.in1
          forecastArrivalsFanOut ~> fcstArrivalsSink

          liveBaseArrivalsSourceSync ~> liveBaseArrivalsDiffing ~> liveBaseArrivalsFanOut
          liveBaseArrivalsFanOut
            .collect { case ArrivalsFeedSuccess(Flights(as), _) if as.nonEmpty => as.toList }
            .conflate[List[Arrival]] { case (acc, incoming) =>
                log.info(s"${acc.length + incoming.length} conflated live base arrivals")
                acc ++ incoming } ~> arrivals.in2
          liveBaseArrivalsFanOut ~> liveBaseArrivalsSink

          liveArrivalsSourceSync ~> arrivalsKillSwitchSync ~> liveArrivalsDiffing ~> liveArrivalsFanOut
          liveArrivalsFanOut
            .collect { case ArrivalsFeedSuccess(Flights(as), _) => as.toList }
            .conflate[List[Arrival]] { case (acc, incoming) =>
                log.info(s"${acc.length + incoming.length} conflated live arrivals")
                acc ++ incoming } ~> arrivals.in3
          liveArrivalsFanOut ~> liveArrivalsSink

          manifestsLiveSourceSync ~> manifestsLiveKillSwitchSync ~> manifestsFanOut

          manifestsFanOut.out(0)
            .collect { case ManifestsFeedSuccess(DqManifests(_, manifests), _) if manifests.nonEmpty => manifests.map(BestAvailableManifest(_)).toList }
            .conflate[List[BestAvailableManifest]] { case (acc, incoming) =>
                log.info(s"${acc.length + incoming.length} conflated API manifests")
                acc ++ incoming } ~> arrivalSplits.in1

          manifestsFanOut.out(1) ~> manifestsSink

          manifestResponsesSource
            .conflate[List[BestAvailableManifest]] { case (acc, incoming) =>
                log.info(s"${acc.length + incoming.length} conflated historic manifests")
                acc ++ incoming } ~> arrivalSplits.in2

          shiftsSourceAsync          ~> shiftsKillSwitchSync ~> staff.in0
          fixedPointsSourceAsync     ~> fixedPointsKillSwitchSync ~> staff.in1
          staffMovementsSourceAsync  ~> movementsKillSwitchSync ~> staff.in2

          arrivals.out ~> arrivalsFanOut ~> arrivalSplits.in0
                          arrivalsFanOut.map { _.toUpdate.values.toList } ~> manifestRequestsSink

          arrivalSplits.out ~> arrivalSplitsFanOut
                               arrivalSplitsFanOut ~> flightsWithSplitsSink
                               arrivalSplitsFanOut
                                 .map(_.arrivalsToRemove.map(ua => RemoveFlight(ua)).toList)
                                 .conflateWithSeed(List(_)) { case (acc, incoming) =>
                                    log.info(s"${acc.length + incoming.length} conflated arrivals for removal sink")
                                    acc :+ incoming }
                                 .mapConcat(_.flatten) ~> arrivalRemovalsSink
                               arrivalSplitsFanOut
                                 .map(_.flightsToUpdate.map(_.apiFlight))
                                 .conflateWithSeed(List(_)) { case (acc, incoming) =>
                                    log.info(s"${acc.length + incoming.length} conflated arrivals for update sink")
                                    acc :+ incoming }
                                 .mapConcat(_.flatten) ~> arrivalUpdatesSink

          actualDesksAndWaitTimesSourceSync ~> deskStatsSink

          staff.out ~> staffFanOut ~> staffSink
                       staffFanOut.map(staffMinutes => UpdatedMillis(staffMinutes.millis)) ~> deploymentRequestSink

          // @formatter:on

          ClosedShape
    }

    RunnableGraph
      .fromGraph(graph)
      .withAttributes(StreamSupervision.resumeStrategyWithLog(RunnableCrunch.getClass.getName))
  }

  def withOnlyDescheduledRemovals(removals: List[RemoveFlight], now: SDateLike): List[RemoveFlight] = {
    val nowMillis = now.millisSinceEpoch
    removals.filterNot(_.flightKey.scheduled <= nowMillis)
  }

  def liveStart(now: () => SDateLike): SDateLike = now().getLocalLastMidnight.addDays(-1)

  def liveEnd(now: () => SDateLike,
              liveStateDaysAhead: Int): SDateLike = now().getLocalNextMidnight.addDays(liveStateDaysAhead)

  def forecastEnd(now: () => SDateLike): SDateLike = now().getLocalNextMidnight.addDays(360)

  def forecastStart(now: () => SDateLike): SDateLike = now().getLocalNextMidnight
}
