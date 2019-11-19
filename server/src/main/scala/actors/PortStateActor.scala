package actors

import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import akka.actor.{Actor, Props}
import akka.pattern.AskableActorRef
import akka.util.Timeout
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.crunch.deskrecs.GetFlights
import services.graphstages.Crunch
import services.graphstages.Crunch.{LoadMinute, Loads}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.postfixOps


object PortStateActor {
  def props(liveStateActor: AskableActorRef, forecastStateActor: AskableActorRef, airportConfig: AirportConfig, expireAfterMillis: Long, now: () => SDateLike, liveDaysAhead: Int): Props =
    Props(new PortStateActor(liveStateActor, forecastStateActor, airportConfig, expireAfterMillis, now, 2))
}

class PortStateActor(liveStateActor: AskableActorRef,
                     forecastStateActor: AskableActorRef,
                     airportConfig: AirportConfig,
                     expireAfterMillis: Long,
                     now: () => SDateLike,
                     liveDaysAhead: Int) extends Actor {
  val log: Logger = LoggerFactory.getLogger(getClass)

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  implicit val timeout: Timeout = new Timeout(1 minute)

  var maybeCrunchActor: Option[AskableActorRef] = None
  var crunchSourceIsReady: Boolean = true
  var maybeSimActor: Option[AskableActorRef] = None
  var simulationActorIsReady: Boolean = true

  val stateDays: mutable.SortedMap[String, AskableActorRef] = mutable.SortedMap[String, AskableActorRef]()

  def actorForDay(day: String): AskableActorRef = {
    stateDays.get(day) match {
      case Some(dayActor) =>
        log.info(s"Got an existing day actor for $day")
        dayActor
      case None =>
        log.info(s"Starting a day actor for $day")
        val forDay: AskableActorRef = context.actorOf(PortStateDayActor.props(day, airportConfig.queues, now))
        stateDays(day) = forDay
        forDay
    }
  }

  def persistUpdates(updates: PortStateMinutes): Unit = {
    val daysToUpdate = updates.minutesUpdated.map(ms => SDate(ms).toISODateOnly).toSet
    daysToUpdate.map { day =>
      actorForDay(day).ask(updates)
    }
  }

  override def receive: Receive = {
    case SetCrunchActor(crunchActor) =>
      log.info(s"Received crunchSourceActor")
      maybeCrunchActor = Option(crunchActor)

    case SetSimulationActor(simActor) =>
      log.info(s"Received simulationSourceActor")
      maybeSimActor = Option(simActor)

    case StreamInitialized => sender() ! Ack

    case StreamCompleted => log.info(s"Stream completed")

    case StreamFailure(t) => log.error(s"Stream failed", t)

    case updates: PortStateMinutes =>
      log.debug(s"Processing incoming PortStateMinutes ${updates.getClass}")

      updates match {
        case fws: FlightsWithSplits => flightMinutesBuffer ++= fws.minutesUpdated
        case drms: DeskRecMinutes => loadMinutesBuffer ++= drms.minutes.map { drm =>
          val lm = LoadMinute(drm)
          (lm.uniqueId, lm)
        }
        case _ =>
      }

      handleCrunchRequest()
      handleSimulationRequest()

      persistUpdates(updates)

    case SetCrunchSourceReady =>
      crunchSourceIsReady = true
      context.self ! HandleCrunchRequest

    case SetSimulationSourceReady =>
      simulationActorIsReady = true
      context.self ! HandleSimulationRequest

    case HandleCrunchRequest =>
      handleCrunchRequest()

    case HandleSimulationRequest =>
      handleSimulationRequest()

    //    case GetState =>
    //      log.debug(s"Received GetState request. Replying with PortState containing ${state.crunchMinutes.count} crunch minutes")
    //      sender() ! Option(state.immutable)

    case GetPortState(startMillis, endMillis) =>
      log.debug(s"Received GetPortState Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")
      val days = daysFromMillis(startMillis, endMillis)
      val updates: Set[Future[Option[PortStateMutable]]] = days.map { day =>
        actorForDay(day).ask(GetPortState(startMillis, endMillis)).asInstanceOf[Future[Option[PortStateMutable]]]
      }
      val replyTo = sender()
      Future.sequence(updates)
        .map(_.reduce[Option[PortStateMutable]] {
          case (None, maybePs) => maybePs
          case (maybePs, None) => maybePs
          case (Some(ps1), Some(ps2)) =>
            ps1.flights ++= ps2.flights.all
            ps1.crunchMinutes ++= ps2.crunchMinutes.all
            ps1.staffMinutes ++= ps2.staffMinutes.all
            Option(ps1.window(SDate(startMillis), SDate(endMillis)).mutable)
        })
        .foreach(maybePsu => {
          replyTo ! maybePsu
        })

    case GetPortStateForTerminal(startMillis, endMillis, terminalName) =>
      log.debug(s"Received GetPortState Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")

      val days = daysFromMillis(startMillis, endMillis)
      val updates: Set[Future[Option[PortStateMutable]]] = days.map { day =>
        actorForDay(day).ask(GetPortState(startMillis, endMillis)).asInstanceOf[Future[Option[PortStateMutable]]]
      }
      val replyTo = sender()
      Future.sequence(updates)
        .map(_.reduce[Option[PortStateMutable]] {
          case (None, maybePs) => maybePs
          case (maybePs, None) => maybePs
          case (Some(ps1), Some(ps2)) =>
            ps1.flights ++= ps2.flights.all
            ps1.crunchMinutes ++= ps2.crunchMinutes.all
            ps1.staffMinutes ++= ps2.staffMinutes.all
            Option(ps1.windowWithTerminalFilter(SDate(startMillis), SDate(endMillis), Seq(terminalName)).mutable)
        })
        .foreach(maybePsu => {
          replyTo ! maybePsu
        })

    case GetUpdatesSince(sinceMillis, startMillis, endMillis) =>
      val replyTo = sender()
      val days = daysFromMillis(startMillis, endMillis)
      val updates: Set[Future[Option[PortStateUpdates]]] = days.map { day =>
        actorForDay(day).ask(GetUpdatesSince(sinceMillis, startMillis, endMillis)).asInstanceOf[Future[Option[PortStateUpdates]]]
      }
      Future.sequence(updates)
        .map(_.reduce[Option[PortStateUpdates]] {
          case (None, maybePsu) => maybePsu
          case (maybePsu, None) => maybePsu
          case (Some(psu1), Some(psu2)) => Option(PortStateUpdates(
            List(psu1.latest, psu2.latest).max,
            psu1.flights ++ psu2.flights,
            (psu1.minutes.map(m => (m.key, m)).toMap ++ psu2.minutes.map(m => (m.key, m)).toMap).values.toSet,
            (psu1.staff.map(m => (m.key, m)).toMap ++ psu2.staff.map(m => (m.key, m)).toMap).values.toSet
          ))
        })
        .foreach(maybePsu => {
          replyTo ! maybePsu
        })

    case GetFlights(startMillis, endMillis) =>
      val start = SDate(startMillis)
      val end = SDate(endMillis)
      log.info(s"Got request for flights between ${start.toISOString()} - ${end.toISOString()}")
      val replyTo = sender()
      val days = daysFromMillis(startMillis, endMillis)
      val updates: Set[Future[FlightsWithSplits]] = days.map { day =>
        println(s"Sending GetFlights(${start.toISOString()}, ${end.toISOString()}) to $day actor")
        actorForDay(day).ask(GetFlights(startMillis, endMillis)).asInstanceOf[Future[FlightsWithSplits]]
      }
      Future.sequence(updates)
        .map(_.reduce[FlightsWithSplits] {
          case (fs1, fs2) => FlightsWithSplits(
            flightsToUpdate = fs1.flightsToUpdate ++ fs2.flightsToUpdate,
            arrivalsToRemove = fs1.arrivalsToRemove ++ fs2.arrivalsToRemove
          )
        })
        .foreach { fs =>
          println(s"Sending flights back to sender")
          replyTo ! fs
        }

    case unexpected => log.warn(s"Got unexpected: $unexpected")
  }

  def daysFromMillis(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch): Set[String] =
    (startMillis until endMillis by Crunch.oneHourMillis).map(hr => SDate(hr).toISODateOnly).toSet

  val flightMinutesBuffer: mutable.Set[MillisSinceEpoch] = mutable.Set[MillisSinceEpoch]()
  val loadMinutesBuffer: mutable.Map[TQM, LoadMinute] = mutable.Map[TQM, LoadMinute]()

  private def handleCrunchRequest(): Unit = (maybeCrunchActor, flightMinutesBuffer.nonEmpty, crunchSourceIsReady) match {
    case (Some(crunchActor), true, true) =>
      crunchSourceIsReady = false
      crunchActor
        .ask(flightMinutesBuffer.toList)(new Timeout(10 minutes))
        .recover {
          case t => log.error("Error sending minutes to crunch", t)
        }
        .onComplete { _ =>
          context.self ! SetCrunchSourceReady
        }
      flightMinutesBuffer.clear()
    case _ => Unit
  }

  private def handleSimulationRequest(): Unit = (maybeSimActor, loadMinutesBuffer.nonEmpty, simulationActorIsReady) match {
    case (Some(simActor), true, true) =>
      simulationActorIsReady = false
      simActor
        .ask(Loads(loadMinutesBuffer.values.toList))(new Timeout(10 minutes))
        .recover {
          case t => log.error("Error sending loads to simulate", t)
        }
        .onComplete { _ =>
          context.self ! SetSimulationSourceReady
        }
      loadMinutesBuffer.clear()
    case _ => Unit
  }
}

case object HandleCrunchRequest

case object HandleSimulationRequest

case object SetCrunchSourceReady

case object SetSimulationSourceReady
