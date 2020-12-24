package actors.daily

import actors._
import actors.acking.AckingReceiver.Ack
import akka.actor.Props
import akka.persistence.{Recovery, SaveSnapshotSuccess, SnapshotSelectionCriteria}
import drt.shared.ArrivalKey
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.dates.UtcDate
import manifests.passengers.{BestAvailableManifest, BestAvailableManifests}
import org.slf4j.{Logger, LoggerFactory}
import scalapb.GeneratedMessage
import server.protobuf.messages.BestAvailableManifest.BestAvailableManifestsMessage
import server.protobuf.messages.VoyageManifest.VoyageManifestsMessage
import services.SDate

object DayHistoricManifestActor {
  def props(date: UtcDate): Props =
    Props(new DayHistoricManifestActor(date.year, date.month, date.day, None))

  def propsPointInTime(date: UtcDate, pointInTime: MillisSinceEpoch): Props =
    Props(new DayHistoricManifestActor(date.year, date.month, date.day, Option(pointInTime)))
}

class DayHistoricManifestActor(
                                year: Int,
                                month: Int,
                                day: Int,
                                maybePointInTime: Option[MillisSinceEpoch]
                              ) extends RecoveryActorLike {

  def now: () => SDate.JodaSDate = () => SDate.now()

  val loggerSuffix: String = maybePointInTime match {
    case None => ""
    case Some(pit) => f"@${SDate(pit).toISOString()}"
  }

  override val log: Logger = LoggerFactory.getLogger(f"$getClass-$year%04d-$month%02d-$day%02d$loggerSuffix")

  override def persistenceId: String = f"manifests-$year-$month%02d-$day%02d"

  override val snapshotBytesThreshold: Int = Sizes.oneMegaByte
  private val maxSnapshotInterval = 250
  override val maybeSnapshotInterval: Option[Int] = Option(maxSnapshotInterval)
  override val recoveryStartMillis: MillisSinceEpoch = now().millisSinceEpoch

  var state: Map[ArrivalKey, BestAvailableManifest] = Map()

  override def recovery: Recovery = maybePointInTime match {
    case None =>
      Recovery(SnapshotSelectionCriteria(Long.MaxValue, maxTimestamp = Long.MaxValue, 0L, 0L))
    case Some(pointInTime) =>
      val criteria = SnapshotSelectionCriteria(maxTimestamp = pointInTime)
      Recovery(fromSnapshot = criteria, replayMax = maxSnapshotInterval)
  }

  override def receiveCommand: Receive = {
    case manifests: BestAvailableManifests =>
      log.info(s"Received ${manifests.manifests.size} manifests to persist")
      updateAndPersist(manifests)

    case GetState =>
      log.debug(s"Received GetState")
      sender() ! BestAvailableManifests(state.values.toList)

    case _: SaveSnapshotSuccess =>
      ackIfRequired()

    case m => log.warn(s"Got unexpected message: $m")
  }

  override def processRecoveryMessage: PartialFunction[Any, Unit] = {

    case vmm@BestAvailableManifestsMessage(Some(createdAt), _) =>
      maybePointInTime match {
        case Some(pit) if pit < createdAt => // ignore messages from after the recovery point.
        case _ =>
          state = state ++ BestAvailableManifestMessageConversion.manifestsFromMessage(vmm).toMap
      }
  }

  override def processSnapshotMessage: PartialFunction[Any, Unit] = {
    case vmm: BestAvailableManifestsMessage =>
      state = BestAvailableManifestMessageConversion.manifestsFromMessage(vmm).toMap
  }

  override def stateToMessage: GeneratedMessage = BestAvailableManifestMessageConversion
    .manifestsToMessage(BestAvailableManifests(state.values.toList))

  def updateAndPersist(bms: BestAvailableManifests): Unit = {

    state = state ++ bms.toMap

    val replyToAndMessage = Option(sender(), Ack)

    persistAndMaybeSnapshotWithAck(BestAvailableManifestMessageConversion.manifestsToMessage(bms), replyToAndMessage)
  }

}
