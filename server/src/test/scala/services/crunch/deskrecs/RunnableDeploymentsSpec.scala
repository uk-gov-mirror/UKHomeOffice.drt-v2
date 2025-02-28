package services.crunch.deskrecs

import actors.MinuteLookupsLike
import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import actors.daily.RequestAndTerminateActor
import actors.minutes.MinutesActorLike.{MinutesLookup, MinutesUpdate}
import actors.minutes.{QueueMinutesActor, StaffMinutesActor}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import drt.shared.CrunchApi.CrunchMinute
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.Map
import scala.concurrent.ExecutionContext


class MockPortStateActorForDeployments(probe: TestProbe, responseDelayMillis: Long = 0L) extends Actor {
  val log: Logger = LoggerFactory.getLogger(getClass)

  override def receive: Receive = {
    case StreamInitialized =>
      sender() ! Ack

    case StreamCompleted =>
      log.info(s"Completed")
      probe.ref ! StreamCompleted

    case StreamFailure(t) =>
      log.error(s"Failed", t)
      probe.ref ! StreamFailure

    case simMins: SimulationMinutes =>
      sender() ! Ack
      probe.ref ! simMins
  }
}

class TestQueueMinutesActor(probe: ActorRef,
                            terminals: Iterable[Terminal],
                            lookup: MinutesLookup[CrunchMinute, TQM],
                            updateMinutes: MinutesUpdate[CrunchMinute, TQM],
                            updatesSubscriber: ActorRef) extends QueueMinutesActor(terminals, lookup, updateMinutes, updatesSubscriber) {

  override def receive: Receive = testReceives

  def testReceives: Receive = {
    case msg =>
      probe ! msg
      super.receive(msg)
  }
}

case class TestMinuteLookups(queueProbe: ActorRef,
                             system: ActorSystem,
                             now: () => SDateLike,
                             expireAfterMillis: Int,
                             queuesByTerminal: Map[Terminal, Seq[Queue]],
                             deploymentsQueueSubscriber: ActorRef)
                            (implicit val ec: ExecutionContext) extends MinuteLookupsLike {
  override val requestAndTerminateActor: ActorRef = system.actorOf(Props(new RequestAndTerminateActor()), "test-minutes-lookup-kill-actor")

  override val queueMinutesActor: ActorRef = system.actorOf(Props(new TestQueueMinutesActor(queueProbe, queuesByTerminal.keys, queuesLookup, updateCrunchMinutes, deploymentsQueueSubscriber)))

  override val staffMinutesActor: ActorRef = system.actorOf(Props(new StaffMinutesActor(queuesByTerminal.keys, staffLookup, updateStaffMinutes)))
}



