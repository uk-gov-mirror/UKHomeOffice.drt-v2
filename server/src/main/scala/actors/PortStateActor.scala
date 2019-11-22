package actors

import java.util.UUID

import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import akka.actor.{Actor, ActorRef, PoisonPill, Props}
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
  def propsStreaming(airportConfig: AirportConfig, expireAfterMillis: Long, now: () => SDateLike): Props =
    Props(new PortStateActor(airportConfig, expireAfterMillis, now, isStreaming = true))

  def propsStatic(airportConfig: AirportConfig, expireAfterMillis: Long, now: () => SDateLike): Props =
    Props(new PortStateActor(airportConfig, expireAfterMillis, now, isStreaming = false))
}

class PortStateActor(airportConfig: AirportConfig,
                     expireAfterMillis: Long,
                     now: () => SDateLike,
                     isStreaming: Boolean) extends Actor {
  val log: Logger = LoggerFactory.getLogger(getClass)

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  implicit val mat: ActorMaterializer = ActorMaterializer()

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
          val daysToDrop = lastQueries.toSeq.sortBy(_._2).take(actorCacheSize - actorCacheCapacity).map(_._1)
          log.info(s"Dropping cached read actors ${daysToDrop.mkString(", ")}")
          daysToDrop.foreach { dayToDrop =>
            stateDays.get(dayToDrop).foreach(_ ? PoisonPill)
            stateDays -= dayToDrop
            lastQueries -= dayToDrop
          }
        }
        log.info(s"Starting a day actor for $day")
        val props = if (isStreaming)
          PortStateDayReadOnlyActor.propsStreaming(day, airportConfig.queues, now)
        else
          PortStateDayReadOnlyActor.propsStatic(day, airportConfig.queues, now)

        val forDay: AskableActorRef = context.actorOf(props, s"port-state-$day-$generateRandomUuid")
        stateDays(day) = forDay
        lastQueries(day) = now().millisSinceEpoch
        forDay
    }
  }

  def generateRandomUuid: String = UUID.randomUUID().toString

  def updateActorForDay(day: String, updates: PortStateMinutes): Future[Any] = {
    log.info(s"Starting a day actor for $day")
    val writeActor: AskableActorRef = dayWriteActor(day)
    writeActor.ask(updates).map { _ =>
      log.info(s"Update finished for $day. Sending PoisonPill")
      writeActor.ask(PoisonPill).map { _ =>
        log.info(s"PoisonPill processed for $day")
      }
    }
  }

  def dayWriteActor(day: String): ActorRef = context.actorOf(PortStateDayActor.props(day, airportConfig.queues, now), writeActorName(day))

  def writeActorName(day: String): String = s"port-state-wo-$day-$generateRandomUuid"

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
      makeRequest2(startMillis, endMillis, request, reducePortStates2(startMillis, endMillis))

    case request@GetPortStateForTerminal(startMillis, endMillis, terminalName) =>
      log.debug(s"Received GetPortStateForTerminal Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")
      val terminalQueues = airportConfig.queues.filterKeys(_ == terminalName)
      makeRequest2(startMillis, endMillis, request, reduceTerminalPortState2(startMillis, endMillis, terminalQueues))

    case request@GetUpdatesSince(_, startMillis, endMillis) =>
      log.debug(s"Received GetUpdatesSince Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")
      makeRequest2(startMillis, endMillis, request, reducePortStateUpdates2(startMillis, endMillis))

    case request@GetFlights(startMillis, endMillis) =>
      log.debug(s"Received GetFlights Request from ${SDate(startMillis).toISOString()} to ${SDate(endMillis).toISOString()}")
      makeRequest2(startMillis, endMillis, request, reduceFlights2(startMillis, endMillis))

    case unexpected => log.warn(s"Got unexpected: $unexpected")
  }

  def makeRequest2[X](startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch, request: Any, reduce: (X, X) => X): Unit = {
    val days = daysFromMillis(startMillis, endMillis)
    val replyTo = sender()

    Source(days)
      .mapAsync(1) { day =>
        actorForDay(day).ask(request).asInstanceOf[Future[X]]
      }
      .reduce(reduce)
      .to(Sink.actorRef(replyTo, "make request finished"))
      .run()
  }

  def reduceTerminalPortState2(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch, terminals: Map[TerminalName, Seq[QueueName]]): (Option[PortState], Option[PortState]) => Option[PortState] =
    (maybePs1: Option[PortState], maybePs2: Option[PortState]) => (maybePs1, maybePs2) match {
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

  def reducePortStates2(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch): (Option[PortState], Option[PortState]) => Option[PortState] =
    (maybePs1: Option[PortState], maybePs2: Option[PortState]) => (maybePs1, maybePs2) match {
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

  def reducePortStateUpdates2(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch): (Option[PortStateUpdates], Option[PortStateUpdates]) => Option[PortStateUpdates] =
    (maybePs1: Option[PortStateUpdates], maybePs2: Option[PortStateUpdates]) => (maybePs1, maybePs2) match {
      case (None, maybePsu) => maybePsu
      case (maybePsu, None) => maybePsu
      case (Some(psu1), Some(psu2)) => Option(PortStateUpdates(
        List(psu1.latest, psu2.latest).max,
        psu1.flights ++ psu2.flights,
        (psu1.minutes.map(m => (m.key, m)).toMap ++ psu2.minutes.map(m => (m.key, m)).toMap).values.toSet,
        (psu1.staff.map(m => (m.key, m)).toMap ++ psu2.staff.map(m => (m.key, m)).toMap).values.toSet
      ))
    }

  def reduceFlights2(startMillis: MillisSinceEpoch, endMillis: MillisSinceEpoch): (FlightsWithSplits, FlightsWithSplits) => FlightsWithSplits =
    (fws1: FlightsWithSplits, fws2: FlightsWithSplits) => (fws1, fws2) match {
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
