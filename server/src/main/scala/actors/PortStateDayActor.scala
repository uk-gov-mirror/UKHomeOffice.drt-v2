package actors

import actors.PortStateMessageConversion._
import actors.acking.AckingReceiver.{Ack, StreamCompleted}
import actors.restore.RestorerWithLegacy
import akka.actor.Props
import akka.persistence._
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{FlightsWithSplits, QueueName, TerminalName}
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import scalapb.GeneratedMessage
import server.protobuf.messages.CrunchState.{CrunchDiffMessage, CrunchStateSnapshotMessage}
import server.protobuf.messages.FlightsMessage.UniqueArrivalMessage
import services.SDate
import services.crunch.deskrecs.GetFlights
import services.graphstages.Crunch

import scala.language.postfixOps


object PortStateDayActor {
  def props(day: String, portQueues: Map[TerminalName, Seq[QueueName]], now: () => SDateLike): Props =
    Props(new PortStateDayActor(day, portQueues, now))
}

class PortStateDayActor(day: String,
                        portQueues: Map[TerminalName, Seq[QueueName]],
                        now: () => SDateLike) extends PersistentActor with RecoveryActorLike with PersistentDrtActor[PortStateMutable] {
  override def persistenceId: String = s"port-state-$day"

  val log: Logger = LoggerFactory.getLogger(persistenceId)

  val restorer = new RestorerWithLegacy[Int, UniqueArrival, ApiFlightWithSplits]

  var state: PortStateMutable = initialState

  def initialState: PortStateMutable = PortStateMutable.empty

  val startMillis: MillisSinceEpoch = SDate(day).millisSinceEpoch
  val endMillis: MillisSinceEpoch = SDate(day).addHours(24).millisSinceEpoch

  def processRecoveryMessage: PartialFunction[Any, Unit] = {
    case diff: CrunchDiffMessage =>
      applyRecoveryDiff(diff, endMillis)
      logRecoveryState()
      bytesSinceSnapshotCounter += diff.serializedSize
      messagesPersistedSinceSnapshotCounter += 1
  }

  override def postRecoveryComplete(): Unit = {
    restorer.finish()
    state.flights ++= restorer.items
    restorer.clear()

    super.postRecoveryComplete()
  }

  def logRecoveryState(): Unit = {
    log.debug(s"Recovery: state contains ${state.flights.count} flights " +
      s", ${state.crunchMinutes.count} crunch minutes " +
      s", ${state.staffMinutes.count} staff minutes ")
  }

  override def stateToMessage: GeneratedMessage = portStateToSnapshotMessage(state)

  override def receiveCommand: Receive = {
//    case psd: PortStateDiff =>
//      if (!psd.isEmpty) {
//        val diffMsg = diffMessage(psd)
//        applyDiff(psd, Long.MaxValue)
//        persistAndMaybeSnapshot(diffMsg)
//      }
//
//      sender() ! Ack

    case updates: PortStateMinutes =>
      val diff = updates.applyTo(state, now().millisSinceEpoch)
      if (!diff.isEmpty) {
        val diffMsg = diffMessage(diff)
        persistAndMaybeSnapshot(diffMsg)
      }

      sender() ! Ack

    case GetState =>
      log.debug(s"Received GetState request. Replying with PortState containing ${state.crunchMinutes.count} crunch minutes")
      sender() ! Option(state.immutable)

    case GetPortState(startMillis, endMillis) =>
      log.debug(s"Received GetPortState(${SDate(startMillis).toISOString()}, ${SDate(startMillis).toISOString()}) request. Replying with PortState containing ${state.crunchMinutes.count} crunch minutes")
      sender() ! Option(state.window(SDate(startMillis), SDate(endMillis)))

    case GetPortStateForTerminal(startMillis, endMillis, terminalName) =>
      log.debug(s"Received GetPortState(${SDate(startMillis).toISOString()}, ${SDate(startMillis).toISOString()}, $terminalName) request. Replying with PortState containing ${state.crunchMinutes.count} crunch minutes")
      sender() ! Option(state.windowWithTerminalFilter(SDate(startMillis), SDate(endMillis), Seq(terminalName)))

    case GetUpdatesSince(sinceMillis, startMillis, endMillis) =>
      sender() ! state.updates(sinceMillis, startMillis, endMillis)

    case GetFlights(startMillis, endMillis) =>
      val start = SDate(startMillis)
      val end = SDate(endMillis)
      log.info(s"Got request for flights between ${start.toISOString()} - ${end.toISOString()}")
      val flightsToSend = state.flights.range(start, end).values.toList
      sender() ! FlightsWithSplits(flightsToSend, List())

    case SaveSnapshotSuccess(SnapshotMetadata(_, seqNr, _)) =>
      log.info(s"Snapshot success for sequenceNr: $seqNr")

    case SaveSnapshotFailure(md, cause) =>
      log.error(s"Save snapshot failure: $md", cause)

    case DeleteSnapshotsSuccess(_) =>
      log.info(s"Purged snapshots")

    case StreamCompleted => log.warn("Received shutdown")

    case unexpected => log.error(s"Received unexpected message $unexpected")
  }

  def diffMessage(diff: PortStateDiff): CrunchDiffMessage = CrunchDiffMessage(
    createdAt = Option(now().millisSinceEpoch),
    crunchStart = Option(0),
    flightsToRemove = diff.flightRemovals.values.map { case RemoveFlight(ua) => UniqueArrivalMessage(Option(ua.number), Option(ua.terminalName), Option(ua.scheduled)) }.toSeq,
    flightsToUpdate = diff.flightUpdates.values.map(FlightMessageConversion.flightWithSplitsToMessage).toList,
    crunchMinutesToUpdate = diff.crunchMinuteUpdates.values.map(crunchMinuteToMessage).toList,
    staffMinutesToUpdate = diff.staffMinuteUpdates.values.map(staffMinuteToMessage).toList
  )

  def stateForPeriod(start: MillisSinceEpoch, end: MillisSinceEpoch): Option[PortState] = Option(state.window(SDate(start), SDate(end)))

  def stateForPeriodForTerminal(start: MillisSinceEpoch, end: MillisSinceEpoch, terminalName: TerminalName): Option[PortState] = Option(state.windowWithTerminalFilter(SDate(start), SDate(end), portQueues.keys.filter(_ == terminalName).toSeq))

  def setStateFromSnapshot(snapshot: CrunchStateSnapshotMessage, timeWindowEnd: Option[SDateLike] = None): Unit = {
    snapshotMessageToState(snapshot, timeWindowEnd, state)
  }

  def applyRecoveryDiff(cdm: CrunchDiffMessage, maxMillis: MillisSinceEpoch): Unit = {
    val (flightRemovals, flightUpdates, crunchMinuteUpdates, staffMinuteUpdates) = crunchDiffFromMessage(cdm, maxMillis)
    val nowMillis = now().millisSinceEpoch
    restorer.update(flightUpdates)
    restorer.removeLegacies(cdm.flightIdsToRemoveOLD)
    restorer.remove(flightRemovals)
    state.applyCrunchDiff(crunchMinuteUpdates, nowMillis)
    state.applyStaffDiff(staffMinuteUpdates, nowMillis)
  }

  def uniqueArrivalFromMessage(uam: UniqueArrivalMessage): UniqueArrival = {
    UniqueArrival(uam.getNumber, uam.getTerminalName, uam.getScheduled)
  }

  def applyDiff(cdm: PortStateDiff, maxMillis: MillisSinceEpoch): Unit = {
    val nowMillis = now().millisSinceEpoch
    state.applyFlightsWithSplitsDiff(cdm.flightRemovals.keys.toSeq, cdm.flightUpdates, nowMillis)
    state.applyCrunchDiff(cdm.crunchMinuteUpdates, nowMillis)
    state.applyStaffDiff(cdm.staffMinuteUpdates, nowMillis)

    state.purgeRecentUpdates(nowMillis - Crunch.oneMinuteMillis * 5)
  }

  def crunchDiffFromMessage(diffMessage: CrunchDiffMessage, maxMillis: MillisSinceEpoch): (Seq[UniqueArrival], Seq[ApiFlightWithSplits], Seq[CrunchMinute], Seq[StaffMinute]) = (
    diffMessage.flightsToRemove.collect {
      case m if portQueues.contains(m.getTerminalName) => uniqueArrivalFromMessage(m)
    },
    diffMessage.flightsToUpdate.collect {
      case m if portQueues.contains(m.getFlight.getTerminal) && m.getFlight.getScheduled < maxMillis => flightWithSplitsFromMessage(m)
    },
    diffMessage.crunchMinutesToUpdate.collect {
      case m if portQueues.contains(m.getTerminalName) && m.getMinute < maxMillis => crunchMinuteFromMessage(m)
    },
    diffMessage.staffMinutesToUpdate.collect {
      case m if portQueues.contains(m.getTerminalName) && m.getMinute < maxMillis => staffMinuteFromMessage(m)
    }
  )

  override val snapshotBytesThreshold: Int = 1024 * 1024

  override def processSnapshotMessage: PartialFunction[Any, Unit] = {
    case unexpected => log.error(s"Received an unexpected snapshot message: ${unexpected.getClass}")
  }
}
