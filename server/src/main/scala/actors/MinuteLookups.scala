package actors

import actors.daily.{RequestAndTerminate, RequestAndTerminateActor, TerminalDayQueuesActor, TerminalDayStaffActor}
import actors.minutes.MinutesActorLike.MinutesLookup
import actors.minutes.{QueueMinutesActor, StaffMinutesActor}
import actors.queues.QueueLikeActor.UpdatedMillis
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import drt.shared.CrunchApi.{CrunchMinute, MillisSinceEpoch, MinutesContainer, StaffMinute}
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared.dates.UtcDate
import drt.shared.{SDateLike, TM, TQM}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

trait MinuteLookupsLike {
  val system: ActorSystem
  implicit val ec: ExecutionContext
  implicit val timeout: Timeout = new Timeout(60 seconds)

  val now: () => SDateLike
  val expireAfterMillis: Int
  val queuesByTerminal: Map[Terminal, Seq[Queue]]
  val requestAndTerminateActor: ActorRef

  val updateCrunchMinutes: ((Terminal, UtcDate), MinutesContainer[CrunchMinute, TQM]) => Future[UpdatedMillis] =
    (terminalDate: (Terminal, UtcDate), container: MinutesContainer[CrunchMinute, TQM]) => {
      val (terminal, date) = terminalDate
      val actor = system.actorOf(TerminalDayQueuesActor.props(terminal, date, now))
      requestAndTerminateActor.ask(RequestAndTerminate(actor, container)).mapTo[UpdatedMillis]
    }

  val updateStaffMinutes: ((Terminal, UtcDate), MinutesContainer[StaffMinute, TM]) => Future[UpdatedMillis] =
    (terminalDate: (Terminal, UtcDate), container: MinutesContainer[StaffMinute, TM]) => {
      val (terminal, date) = terminalDate
      val actor = system.actorOf(TerminalDayStaffActor.props(terminal, date, now))
      requestAndTerminateActor.ask(RequestAndTerminate(actor, container)).mapTo[UpdatedMillis]
    }

  val queuesLookup: MinutesLookup[CrunchMinute, TQM] = (terminalDate: (Terminal, UtcDate), maybePit: Option[MillisSinceEpoch]) => {
    val (terminal, date) = terminalDate
    val props = maybePit match {
      case None => TerminalDayQueuesActor.props(terminal, date, now)
      case Some(pointInTime) => TerminalDayQueuesActor.propsPointInTime(terminal, date, now, pointInTime)
    }
    val actor = system.actorOf(props)
    requestAndTerminateActor.ask(RequestAndTerminate(actor, GetState)).mapTo[Option[MinutesContainer[CrunchMinute, TQM]]]
  }

  val staffLookup: MinutesLookup[StaffMinute, TM] = (terminalDate: (Terminal, UtcDate), maybePit: Option[MillisSinceEpoch]) => {
    val (terminal, date) = terminalDate
    val props = maybePit match {
      case None => TerminalDayStaffActor.props(terminal, date, now)
      case Some(pointInTime) => TerminalDayStaffActor.propsPointInTime(terminal, date, now, pointInTime)
    }
    val actor = system.actorOf(props)
    requestAndTerminateActor.ask(RequestAndTerminate(actor, GetState)).mapTo[Option[MinutesContainer[StaffMinute, TM]]]
  }

  def queueMinutesActor: ActorRef

  def staffMinutesActor: ActorRef
}

case class MinuteLookups(system: ActorSystem,
                         now: () => SDateLike,
                         expireAfterMillis: Int,
                         queuesByTerminal: Map[Terminal, Seq[Queue]],
                         deploymentsQueueActor: ActorRef)
                        (implicit val ec: ExecutionContext) extends MinuteLookupsLike {
  override val requestAndTerminateActor: ActorRef = system.actorOf(Props(new RequestAndTerminateActor()), "minutes-lookup-kill-actor")

  override val queueMinutesActor: ActorRef = system.actorOf(Props(new QueueMinutesActor(queuesByTerminal.keys, queuesLookup, updateCrunchMinutes, deploymentsQueueActor)))

  override val staffMinutesActor: ActorRef = system.actorOf(Props(new StaffMinutesActor(queuesByTerminal.keys, staffLookup, updateStaffMinutes)))
}
