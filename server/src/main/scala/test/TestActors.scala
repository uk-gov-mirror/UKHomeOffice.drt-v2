package test

import actors.Sizes.oneMegaByte
import actors._
import actors.acking.AckingReceiver.Ack
import akka.actor.{ActorRef, PoisonPill, Props}
import akka.pattern.AskableActorRef
import akka.persistence.SnapshotSelectionCriteria
import drt.shared.FlightsApi.{QueueName, TerminalName}
import drt.shared.{AirportConfig, SDateLike}
import slickdb.ArrivalTable

import scala.collection.mutable


object TestActors {

  case object ResetActor

  case class TestForecastBaseArrivalsActor(now: () => SDateLike, expireAfterMillis: Long)
    extends ForecastBaseArrivalsActor(oneMegaByte, now, expireAfterMillis) {

    def reset: Receive = {
      case ResetActor => state.clear()
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  case class TestForecastPortArrivalsActor(now: () => SDateLike, expireAfterMillis: Long)
    extends ForecastPortArrivalsActor(oneMegaByte, now, expireAfterMillis) {

    def reset: Receive = {
      case ResetActor => state.clear()
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  case class TestLiveArrivalsActor(now: () => SDateLike, expireAfterMillis: Long)
    extends LiveArrivalsActor(oneMegaByte, now, expireAfterMillis) {

    def reset: Receive = {
      case ResetActor => state.clear()
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  case class TestVoyageManifestsActor(now: () => SDateLike, expireAfterMillis: Long, snapshotInterval: Int)
    extends VoyageManifestsActor(oneMegaByte, now, expireAfterMillis, Option(snapshotInterval)) {

    def reset: Receive = {
      case ResetActor => state = initialState
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  case class TestShiftsActor(override val now: () => SDateLike, override val expireBefore: () => SDateLike) extends ShiftsActor(now, expireBefore) {

    def reset: Receive = {
      case ResetActor =>
        state = initialState
        subscribers = List()
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  case class TestFixedPointsActor(now: () => SDateLike) extends FixedPointsActor(now) {

    def reset: Receive = {
      case ResetActor =>
        state = initialState
        subscribers = List()
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  case class TestStaffMovementsActor(override val now: () => SDateLike, override val expireBefore: () => SDateLike) extends StaffMovementsActor(now, expireBefore) {

    def reset: Receive = {
      case ResetActor =>
        state = initialState
        subscribers = List()
    }

    override def receiveRecover: Receive = {
      case m => log.info(logMessage(m))
    }

    override def receiveCommand: Receive = reset orElse super.receiveCommand
  }

  case class TestAggregatedArrivalsActor() extends AggregatedArrivalsActor("LHR", ArrivalTable("LHR", PostgresTables)) {
    def reset: Receive = {
      case ResetActor => Unit
    }

    override def receive: Receive = reset orElse super.receive
  }

  object TestPortStateActor {
    def propsStreaming(airportConfig: AirportConfig, expireAfterMillis: Long, now: () => SDateLike): Props =
      Props(new TestPortStateActor(airportConfig, expireAfterMillis, now, isStreaming = true))

    def propsStatic(airportConfig: AirportConfig, expireAfterMillis: Long, now: () => SDateLike): Props =
      Props(new TestPortStateActor(airportConfig, expireAfterMillis, now, isStreaming = false))
  }

  case class TestPortStateActor(airportConfig: AirportConfig, expireAfterMillis: Long, now: () => SDateLike, isStreaming: Boolean)
    extends PortStateActor(airportConfig, expireAfterMillis, now, isStreaming) {

    val daysToReset: mutable.Set[String] = mutable.Set[String]()

    override def dayWriteActor(day: String): ActorRef = {
      println(s"Adding $day to daysToReset")
      daysToReset += day
      testDayWriteActor(day)
    }

    def testDayWriteActor(day: String): ActorRef = context.actorOf(TestPortStateDayActor.props(day, airportConfig.queues, now), writeActorName(day))

    def reset: Receive = {
      case ResetActor =>
        log.info(s"Clearing ${daysToReset.size} days: ${daysToReset.mkString(", ")}")
        daysToReset.foreach { day =>
          log.info(s"Clearing $day")
          val actorRef: AskableActorRef = testDayWriteActor(day)
          actorRef.ask(ResetActor).foreach { _ =>
            actorRef ? PoisonPill
          }
        }
        daysToReset.clear()

        stateDays.foreach { case (_, actor) => actor ? PoisonPill }
        stateDays.clear()

        lastQueries.clear()
    }

    override def receive: Receive = reset orElse super.receive
  }

  object TestPortStateDayActor {
    def props(day: String, portQueues: Map[TerminalName, Seq[QueueName]], now: () => SDateLike): Props =
      Props(new TestPortStateDayActor(day, portQueues, now))
  }

  class TestPortStateDayActor(day: String,
                              portQueues: Map[TerminalName, Seq[QueueName]],
                              now: () => SDateLike) extends PortStateDayActor(day, portQueues, now) {
    def testReceive: Receive = {
      case ResetActor =>
        log.info(s"Clearing state and deleting messages & snapshots")
        state.clear()
        deleteMessages(Long.MaxValue)
        deleteSnapshots(SnapshotSelectionCriteria(Long.MaxValue, Long.MaxValue, 0L, 0L))

        log.info("Deleted messages & snapshots. Acking back")

        sender() ! Ack
    }

    override def receiveCommand: Receive = testReceive orElse super.receiveCommand
  }

  def logMessage(m: Any): String = s"Got this message: ${m.getClass} but not doing anything because this is a test."
}
