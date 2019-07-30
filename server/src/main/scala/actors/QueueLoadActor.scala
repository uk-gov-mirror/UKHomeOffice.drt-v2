package actors

import akka.persistence._
import drt.shared.CrunchApi.LoadMinute
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import server.protobuf.messages.QueueLoad.{QueueLoadMessage, QueueLoadsMessage}
import services.graphstages.Crunch.Loads


class QueueLoadActor() extends PersistentActor {

  val persistenceId: String = "queue-load"

  val log: Logger = LoggerFactory.getLogger(getClass)

  def queueLoadsToMessage(queueLoads: Map[TQM, LoadMinute]): QueueLoadsMessage = QueueLoadsMessage(
    queueLoads.map { case (_, lm) => loadMinuteToMessage(lm) }.toSeq)

  def loadMinuteToMessage(lm: LoadMinute) = QueueLoadMessage(
    Option(lm.terminalName), Option(lm.queueName), Option(lm.minute), Option(lm.workLoad))

  override def receiveCommand: Receive = {
    case Loads(newLoads) =>
      if (newLoads.exists(_._2.minute < 100000000)) println(s"Got some dodgy minutes: ${newLoads.find(_._2.minute < 100000000).get}")
      val newLoadsMessage = queueLoadsToMessage(newLoads)

      persist(newLoadsMessage) { _ =>
        log.info(s"Persisting QueueLoadsMessage of ${newLoadsMessage.serializedSize} bytes")
      }

    case other => log.info(s"Unexpected receive message ${other.getClass}")
  }

  override def receiveRecover: Receive = {
    case other => log.info(s"Unexpected recover message ${other.getClass}")
  }
}
