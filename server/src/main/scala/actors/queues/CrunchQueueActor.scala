package actors.queues

import drt.shared.SDateLike


class CrunchQueueActor(now: () => SDateLike,
                       crunchOffsetMinutes: Int,
                       durationMinutes: Int) extends QueueLikeActor(now,  crunchOffsetMinutes, durationMinutes) {
  override val persistenceId: String = "crunch-queue"
}
