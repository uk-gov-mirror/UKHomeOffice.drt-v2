package actors

import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import akka.actor.{Actor, PoisonPill, Props}
import akka.pattern.AskableActorRef
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.Timeout
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{FlightsWithSplits, QueueName, TerminalName}
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.crunch.deskrecs.GetFlights
import services.graphstages.Crunch
import services.graphstages.Crunch.{LoadMinute, Loads}

import scala.collection.{immutable, mutable}
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

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  //  implicit val mat: ActorMaterializer = ActorMaterializer()

  implicit val timeout: Timeout = new Timeout(1 minute)

  var maybeCrunchActor: Option[AskableActorRef] = None
  var crunchSourceIsReady: Boolean = true
  var maybeSimActor: Option[AskableActorRef] = None
  var simulationActorIsReady: Boolean = true

  val stateDays: mutable.SortedMap[String, AskableActorRef] = mutable.SortedMap[String, AskableActorRef]()
  val lastQueries: mutable.Map[String, MillisSinceEpoch] = mutable.Map[String, MillisSinceEpoch]()
  val actorCacheCapacity = 7

  def actorForDay(day: String): AskableActorRef = {
    stateDays.get(day) match {
      case Some(dayActor) =>
        log.info(s"Got an existing day actor for $day")
        dayActor
      case None =>
        val actorCacheSize = stateDays.size
        if (actorCacheSize > actorCacheCapacity) {
          val daysToDrop = lastQueries.toSeq.sortBy(_._2).take(actorCacheSize - actorCacheCapacity)
          log.info(s"Dropping cached read actors ${daysToDrop.mkString(", ")}")
          daysToDrop.foreach { case (dayToDrop, _) =>
            stateDays.get(dayToDrop).foreach(_ ? PoisonPill)
            stateDays -= dayToDrop
            lastQueries -= dayToDrop
          }
        }
        log.info(s"Starting a day actor for $day")
        val forDay: AskableActorRef = context.actorOf(PortStateDayReadOnlyActor.props(day, airportConfig.queues, now), s"port-state-streaming-$day")
        stateDays(day) = forDay
        lastQueries(day) = now().millisSinceEpoch
        forDay
    }
  }

  def updateActorForDay(day: String, updates: PortStateMinutes): Future[Any] = {
    log.info(s"Starting a day actor for $day")
    val forDay: AskableActorRef = context.actorOf(PortStateDayActor.props(day, airportConfig.queues, now), s"port-state-wo-$day")
    forDay.ask(updates).map { _ =>
      log.info(s"Update finished for $day. Sending PoisonPill")
      forDay.ask(PoisonPill).map { _ =>
        log.info(s"PoisonPill processed for $day")
      }
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

    case (day: String, updates: PortStateMinutes) =>
      log.info(s"Processing incoming PortStateMinutes ${updates.getClass}")

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

      val replyTo = sender()

      updateActorForDay(day, updates).foreach { _ =>
        log.info(s"Acking back to sender")
        replyTo ! Ack
      }

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

    case request@GetPortState(startMillis, endMillis) =>
      log.debug(s"Received GetPortState Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")
      makeRequest(startMillis, endMillis, request, reducePortStates(startMillis, endMillis))

    case request@GetPortStateForTerminal(startMillis, endMillis, terminalName) =>
      log.debug(s"Received GetPortStateForTerminal Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")
      val terminalQueues = airportConfig.queues.filterKeys(_ == terminalName)
      makeRequest(startMillis, endMillis, request, reduceTerminalPortState(startMillis, endMillis, terminalQueues))

    case request@GetUpdatesSince(_, startMillis, endMillis) =>
      log.debug(s"Received GetUpdatesSince Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")
      makeRequest(startMillis, endMillis, request, reducePortStateUpdates(startMillis, endMillis))

    case request@GetFlights(startMillis, endMillis) =>
      log.debug(s"Received GetFlights Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")
      makeRequest(startMillis, endMillis, request, reduceFlights(startMillis, endMillis))

    case unexpected => log.warn(s"Got unexpected: $unexpected")
  }

  def makeRequest[X](startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch, request: Any, reduce: Iterable[X] => X): Unit = {
    val days = daysFromMillis(startMillis, endMillis)
    val updates: Set[Future[X]] = days.map { day =>
      actorForDay(day).ask(request).asInstanceOf[Future[X]]
    }
    val replyTo = sender()
    Future.sequence(updates)
      .map(reduce)
      .foreach { maybePsu =>
        log.info(s"Sending $request response")
        replyTo ! maybePsu
      }
  }

  def reduceTerminalPortState(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch, terminals: Map[TerminalName, Seq[QueueName]]): Iterable[Option[PortState]] => Option[PortState] =
    (maybePortStates: Iterable[Option[PortState]]) => maybePortStates.reduce[Option[PortState]] {
      case (None, maybePs) => maybePs
      case (maybePs, None) => maybePs
      case (Some(ps1), Some(ps2)) =>
        PortState(
          ps1.flights ++ ps2.flights,
          ps1.crunchMinutes ++ ps2.crunchMinutes,
          ps1.staffMinutes ++ ps2.staffMinutes
        )
        Option(ps1.windowWithTerminalFilter(SDate(startMillis), SDate(endMillis), terminals))
    }

  def reducePortStates(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch): Iterable[Option[PortState]] => Option[PortState] =
    (maybePortStates: Iterable[Option[PortState]]) => maybePortStates.reduce[Option[PortState]] {
      case (None, maybePs) => maybePs
      case (maybePs, None) => maybePs
      case (Some(ps1), Some(ps2)) =>
        PortState(
          ps1.flights ++ ps2.flights,
          ps1.crunchMinutes ++ ps2.crunchMinutes,
          ps1.staffMinutes ++ ps2.staffMinutes
        )
        Option(ps1.window(SDate(startMillis), SDate(endMillis)))
    }

  def reducePortStateUpdates(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch): Iterable[Option[PortStateUpdates]] => Option[PortStateUpdates] =
    (maybePortStates: Iterable[Option[PortStateUpdates]]) => maybePortStates.reduce[Option[PortStateUpdates]] {
      case (None, maybePsu) => maybePsu
      case (maybePsu, None) => maybePsu
      case (Some(psu1), Some(psu2)) => Option(PortStateUpdates(
        List(psu1.latest, psu2.latest).max,
        psu1.flights ++ psu2.flights,
        (psu1.minutes.map(m => (m.key, m)).toMap ++ psu2.minutes.map(m => (m.key, m)).toMap).values.toSet,
        (psu1.staff.map(m => (m.key, m)).toMap ++ psu2.staff.map(m => (m.key, m)).toMap).values.toSet
      ))
    }

  def reduceFlights(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch): Iterable[FlightsWithSplits] => FlightsWithSplits =
    (flights: Iterable[FlightsWithSplits]) => flights.reduce[FlightsWithSplits] {
      case (fs1, fs2) => FlightsWithSplits(
        flightsToUpdate = fs1.flightsToUpdate ++ fs2.flightsToUpdate,
        arrivalsToRemove = fs1.arrivalsToRemove ++ fs2.arrivalsToRemove
      )
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
