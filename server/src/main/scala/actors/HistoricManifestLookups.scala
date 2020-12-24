package actors

import actors.daily.{DayHistoricManifestActor, RequestAndTerminate, RequestAndTerminateActor}
import actors.minutes.MinutesActorLike.{HistoricManifestLookup, HistoricManifestsUpdate}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.dates.UtcDate
import manifests.passengers.BestAvailableManifests

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

trait HistoricManifestLookupsLike {
  val system: ActorSystem
  implicit val ec: ExecutionContext
  implicit val timeout: Timeout = new Timeout(60 seconds)

  val requestAndTerminateActor: ActorRef

  val updateManifests: HistoricManifestsUpdate = (date: UtcDate, vms: BestAvailableManifests) => {
    val actor = system.actorOf(DayHistoricManifestActor.props(date))
    system.log.info(s"About to update $date with ${vms.manifests.size} manifests")
    requestAndTerminateActor.ask(RequestAndTerminate(actor, vms)).mapTo[Seq[MillisSinceEpoch]]
  }

  val manifestsByDayLookup: HistoricManifestLookup = (date: UtcDate, maybePit: Option[MillisSinceEpoch]) => {
    val props = maybePit match {
      case None => DayHistoricManifestActor.props(date)
      case Some(pointInTime) => DayHistoricManifestActor.propsPointInTime(date, pointInTime)
    }
    val actor = system.actorOf(props)
    requestAndTerminateActor.ask(RequestAndTerminate(actor, GetState)).mapTo[BestAvailableManifests]
  }

}

case class HistoricManifestLookups(system: ActorSystem)(implicit val ec: ExecutionContext) extends HistoricManifestLookupsLike {
  override val requestAndTerminateActor: ActorRef = system
    .actorOf(Props(new RequestAndTerminateActor()), "historic-manifests-lookup-kill-actor")
}
