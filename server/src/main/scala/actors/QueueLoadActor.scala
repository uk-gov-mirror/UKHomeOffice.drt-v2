package actors

import akka.persistence._
import com.trueaccord.scalapb.GeneratedMessage
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import server.protobuf.messages.QueueLoad.{QueueLoadMessage, QueueLoadsMessage}
import services.graphstages.Crunch
import services.graphstages.Crunch.{LoadMinute, Loads}

import scala.collection.immutable.SortedMap


class QueueLoadActor(now: () => SDateLike, expireAfterMillis: Long)
  extends RecoveryActorLike with PersistentDrtActor[SortedMap[TQM, LoadMinute]] {

  override val log: Logger = LoggerFactory.getLogger(getClass)
  override val snapshotBytesThreshold: Int = 10 * Sizes.oneMegaByte
  override def persistenceId: String = "queue-load"

  var state: SortedMap[TQM, LoadMinute] = SortedMap[TQM, LoadMinute]()

  override def initialState: SortedMap[TQM, LoadMinute] = SortedMap[TQM, LoadMinute]()

  def processSnapshotMessage: PartialFunction[Any, Unit] = {
    case QueueLoadsMessage(queueLoads) =>
      state = SortedMap[TQM, LoadMinute]() ++ queueLoadMessageToLoadMinutes(queueLoads)
  }

  private def queueLoadMessageToLoadMinutes(queueLoads: Seq[QueueLoadMessage]): List[(TQM, LoadMinute)] = queueLoads
    .foldLeft(List[(TQM, LoadMinute)]()) {
      case (loadsSoFar, msg) =>
        val loadMinute = LoadMinute(msg.getTerminal, msg.getQueue, msg.getPax, msg.getWork, msg.getMinute)
        (loadMinute.key, loadMinute) :: loadsSoFar
    }

  def processRecoveryMessage: PartialFunction[Any, Unit] = {
    case qlm@QueueLoadsMessage(queueLoads) =>
      val newLoads = queueLoadMessageToLoadMinutes(queueLoads)
      state = state ++ newLoads
      bytesSinceSnapshotCounter += qlm.serializedSize
      messagesPersistedSinceSnapshotCounter += 1
  }

  override def postRecoveryComplete(): Unit = {
    state = Crunch.purgeExpired(state, now, expireAfterMillis.toInt)
    logRecoveryMessage(s"restored state to snapshot. ${state.size} minutes of queue load")
    super.postRecoveryComplete()
  }

  override def stateToMessage: GeneratedMessage = queueLoadsToMessage(state)

  def queueLoadsToMessage(queueLoads: SortedMap[TQM, LoadMinute]): QueueLoadsMessage = QueueLoadsMessage(queueLoads.map {
    case (_, lm) => QueueLoadMessage(Option(lm.terminalName), Option(lm.queueName), Option(lm.minute), Option(lm.paxLoad), Option(lm.workLoad))
  }.toSeq)

  override def receiveCommand: Receive = {
    case Loads(newLoads) =>
      state = state ++ newLoads
      persistAndMaybeSnapshot(queueLoadsToMessage(state))

    case SaveSnapshotSuccess(md) =>
      log.info(s"Save snapshot success: $md")

    case SaveSnapshotFailure(md, cause) =>
      log.info(s"Save snapshot failure: $md, $cause")

    case other =>
      log.info(s"Received unexpected message ${other.getClass}")
  }
}
