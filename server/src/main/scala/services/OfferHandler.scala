package services

import akka.actor.Scheduler
import akka.pattern.after
import akka.stream.scaladsl.SourceQueueWithComplete
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object OfferHandler {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def offerWithRetries[A](queue: SourceQueueWithComplete[A], thingToOffer: A, retries: Int, maybeOnSuccess: Option[() => Unit] = None)(implicit ec: ExecutionContext, s: Scheduler): Unit = {
    val eventualResult = queue.offer(thingToOffer)

    maybeOnSuccess.foreach(onSuccess => eventualResult.foreach(_ => onSuccess()))

    Retry.retry(RetryDelays.fibonacci.drop(2), retries, 5 seconds)(() => eventualResult).failed.foreach(throwable =>
      log.error(s"Failed to enqueue ${thingToOffer.getClass} after . $retries", throwable))
  }
}

object Retry {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def retry[T](delay: Seq[FiniteDuration], retries: Int, defaultDelay: FiniteDuration)(futureToRetry: () => Future[T])(implicit ec: ExecutionContext, s: Scheduler): Future[T] = futureToRetry()
    .recoverWith {
      case _ if retries > 0 =>
        val nextDelayDuration = delay.headOption.getOrElse(defaultDelay)
        log.warn(s"Future failed. Trying again after $nextDelayDuration. $retries retries remaining")
        after(nextDelayDuration, s)(retry(delay.tail, retries - 1, defaultDelay)(futureToRetry))
    }
}

object RetryDelays {
  val fibonacci: Stream[FiniteDuration] = 0.seconds #:: 1.seconds #:: (fibonacci zip fibonacci.tail).map { t => t._1 + t._2 }
}
