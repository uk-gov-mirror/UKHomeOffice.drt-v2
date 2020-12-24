package actors.queues

import actors.acking.AckingReceiver.Ack
import actors.minutes.MinutesActorLike.{HistoricManifestLookup, HistoricManifestsUpdate, ProcessNextUpdateRequest}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import drt.shared.dates.UtcDate
import manifests.passengers.BestAvailableManifests

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.language.postfixOps

object HistoricManifestRouterActor {
//
//  def manifestsByDaySource(manifestsByDayLookup: ManifestLookup)
//                          (start: SDateLike,
//                           end: SDateLike,
//                           maybePit: Option[MillisSinceEpoch]): Source[VoyageManifests, NotUsed] = {
//    DateRange.utcDateRangeSource(start, end)
//      .mapAsync(1) {
//        date =>
//          manifestsByDayLookup(date, maybePit)
//      }
//  }
//
//  def runAndCombine(source: Future[Source[VoyageManifests, NotUsed]])(implicit mat: ActorMaterializer, ec: ExecutionContext): Future[VoyageManifests] = source
//    .flatMap(
//      _.runWith(Sink.reduce[VoyageManifests](_ ++ _))
//    )
//
  def props(manifestLookup: HistoricManifestLookup, manifestsUpdate: HistoricManifestsUpdate) = Props(
    new HistoricManifestRouterActor(manifestLookup, manifestsUpdate)
  )
}


class HistoricManifestRouterActor(manifestLookup: HistoricManifestLookup, manifestsUpdate: HistoricManifestsUpdate) extends Actor with ActorLogging {


  implicit val dispatcher: ExecutionContextExecutor = context.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer.create(context)
  implicit val timeout: Timeout = new Timeout(60 seconds)

  var updateRequestsQueue: List[(ActorRef, BestAvailableManifests)] = List()
  var processingRequest: Boolean = false

  override def receive: Receive = {

    case manifests: BestAvailableManifests =>
      updateRequestsQueue = (sender(), manifests) :: updateRequestsQueue

      log.info(s"About to route ${manifests.manifests.size} historic manifests")

      self ! ProcessNextUpdateRequest

    //    case PointInTimeQuery(pit, GetStateForDateRange(startMillis, endMillis)) =>
    //      sender() ! HistoricManifestRouterActor.manifestsByDaySource(manifestLookup)(SDate(startMillis), SDate(endMillis), Option(pit))
    //
    //    case GetStateForDateRange(startMillis, endMillis) =>
    //      sender() ! HistoricManifestRouterActor.manifestsByDaySource(manifestLookup)(SDate(startMillis), SDate(endMillis), None)

    case ProcessNextUpdateRequest =>
      if (!processingRequest) {
        updateRequestsQueue match {
          case (replyTo, vms) :: tail =>
            handleUpdatesAndAck(vms, replyTo)
            updateRequestsQueue = tail
          case Nil =>
            log.debug("Update requests queue is empty. Nothing to do")
        }
      }

    case unexpected => log.warning(s"Got an unexpected message: $unexpected")
  }

  def handleUpdatesAndAck(bms: BestAvailableManifests,
                          replyTo: ActorRef): Unit = {
    processingRequest = true
    Future.sequence(
      manifestsByDay(bms)
        .map {
          case (date, vms) => manifestsUpdate(date, vms)
        }
    )
      .onComplete { _ =>
        processingRequest = false
        replyTo ! Ack
        self ! ProcessNextUpdateRequest
      }
  }

  def manifestsByDay(bms: BestAvailableManifests): Map[UtcDate, BestAvailableManifests] = bms
    .manifests
    .groupBy(_.scheduled.toUtcDate)
    .map {
      case (utcDate, manifests) => utcDate -> BestAvailableManifests(manifests)
    }
}
