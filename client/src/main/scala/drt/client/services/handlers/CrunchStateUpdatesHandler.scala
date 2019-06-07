package drt.client.services.handlers

import diode._
import diode.data._
import diode.Implicits.runAfterImpl
import drt.client.actions.Actions._
import drt.client.logger._
import drt.client.services._
import drt.shared.{ApiFlightWithSplits, CrunchApi}
import drt.shared.CrunchApi._
import org.scalajs.dom
import upickle.default.read

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class CrunchStateUpdatesHandler[M](getCurrentViewMode: () => ViewMode,
                                   modelRW: ModelRW[M, (Pot[CrunchState], MillisSinceEpoch)]) extends LoggingActionHandler(modelRW) {
  val liveRequestFrequency: FiniteDuration = 2 seconds
  val forecastRequestFrequency: FiniteDuration = 5 seconds

  val thirtySixHoursInMillis: Long = 1000L * 60 * 60 * 36

  protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case GetCrunchStateUpdates(viewMode) =>
      val (_, lastUpdateMillis) = modelRW.value
      val startMillis = viewMode.dayStart.millisSinceEpoch
      val endMillis = startMillis + thirtySixHoursInMillis
      val updateRequestFuture = DrtApi.get(s"crunch?start=$startMillis&end=$endMillis&since=$lastUpdateMillis")

      val eventualAction = processUpdatesRequest(viewMode, updateRequestFuture)

      effectOnly(Effect(eventualAction))

    case UpdateCrunchStateFromUpdates(viewMode, _) if viewMode.isDifferentTo(getCurrentViewMode()) =>
      log.info(s"Ignoring out of date view response")
      noChange

    case UpdateCrunchStateFromUpdates(viewMode, crunchUpdates) =>
      modelRW.value match {
        case (Ready(existingState), _) =>
          val newState = updateStateFromUpdates(viewMode.dayStart.millisSinceEpoch, crunchUpdates, existingState)
          val scheduledUpdateRequest = Effect(Future(ScheduleCrunchUpdateRequest(viewMode)))
          val newOriginCodes = crunchUpdates.flights.map(_.apiFlight.Origin) -- existingState.flights.map(_.apiFlight.Origin)
          val effects = if (newOriginCodes.nonEmpty) scheduledUpdateRequest + Effect(Future(GetAirportInfos(newOriginCodes))) else scheduledUpdateRequest

          updated((Ready(newState), crunchUpdates.latest), effects)

        case _ =>
          log.warn(s"No existing state to apply updates to. Ignoring")
          noChange
      }


    case ScheduleCrunchUpdateRequest(viewMode) if viewMode.isDifferentTo(getCurrentViewMode()) =>
      log.info(s"Ignoring out of date schedule updates request")
      noChange

    case ScheduleCrunchUpdateRequest(viewMode) => effectOnly(getCrunchUpdatesAfterDelay(viewMode))
  }

  def processUpdatesRequest(viewMode: ViewMode, call: Future[dom.XMLHttpRequest]): Future[Action] = {
    call
      .map(r => read[Either[CrunchStateError, Option[CrunchUpdates]]](r.responseText))
      .map {
        case Right(Some(cu)) => UpdateCrunchStateFromUpdates(viewMode, cu)
        case Right(None) => ScheduleCrunchUpdateRequest(viewMode)
        case Left(error) =>
          log.error(s"Failed to GetCrunchStateUpdates ${error.message}.")

          if (viewMode.isDifferentTo(getCurrentViewMode())) {
            log.info(s"The view appears to have changed. No need to retry")
            NoAction
          } else {
            log.info(s"Re-requesting after ${PollDelay.recoveryDelay}")
            RetryActionAfter(GetCrunchStateUpdates(viewMode), PollDelay.recoveryDelay)
          }
      }
      .recoverWith {
        case throwable =>
          log.error(s"Call to crunch-state failed (${throwable.getMessage}. Re-requesting after ${PollDelay.recoveryDelay}")
          Future(RetryActionAfter(GetCrunchStateUpdates(viewMode), PollDelay.recoveryDelay))
      }
  }

  def updateStateFromUpdates(startMillis: MillisSinceEpoch, crunchUpdates: CrunchUpdates, existingState: CrunchState): CrunchState = {
    val flights = updateAndTrimFlights(crunchUpdates, existingState, startMillis)
    val minutes = updateAndTrimCrunch(crunchUpdates, existingState, startMillis)
    val staff = updateAndTrimStaff(crunchUpdates, existingState, startMillis)
    CrunchState(flights = flights, crunchMinutes = minutes, staffMinutes = staff)
  }

  def updateAndTrimCrunch(crunchUpdates: CrunchUpdates, existingState: CrunchState, keepFromMillis: MillisSinceEpoch): Set[CrunchApi.CrunchMinute] = {
    val relevantMinutes = existingState.crunchMinutes.filter(_.minute >= keepFromMillis)
    val existingMinutesByTqm = relevantMinutes.map(cm => ((cm.terminalName, cm.queueName, cm.minute), cm)).toMap
    val minutes = crunchUpdates.minutes.foldLeft(existingMinutesByTqm) {
      case (soFar, newCm) => soFar.updated((newCm.terminalName, newCm.queueName, newCm.minute), newCm)
    }.values.toSet
    minutes
  }

  def updateAndTrimStaff(crunchUpdates: CrunchUpdates, existingState: CrunchState, keepFromMillis: MillisSinceEpoch): Set[CrunchApi.StaffMinute] = {
    val relevantMinutes = existingState.staffMinutes.filter(_.minute >= keepFromMillis)
    val existingMinutesByTqm = relevantMinutes.map(cm => ((cm.terminalName, cm.minute), cm)).toMap
    val minutes = crunchUpdates.staff.foldLeft(existingMinutesByTqm) {
      case (soFar, newCm) => soFar.updated((newCm.terminalName, newCm.minute), newCm)
    }.values.toSet
    minutes
  }

  def updateAndTrimFlights(crunchUpdates: CrunchUpdates, existingState: CrunchState, keepFromMillis: MillisSinceEpoch): Set[ApiFlightWithSplits] = {
    val thirtyMinutesMillis = 30 * 60000
    val relevantFlights = existingState.flights.filter(_.apiFlight.PcpTime.isDefined).filter(keepFromMillis - thirtyMinutesMillis <= _.apiFlight.PcpTime.getOrElse(0L))
    val flights = crunchUpdates.flights.foldLeft(relevantFlights) {
      case (soFar, newFlight) =>
        val withoutOldFlight = soFar.filterNot(_.apiFlight.uniqueId == newFlight.apiFlight.uniqueId)
        withoutOldFlight + newFlight
    }
    flights
  }

  def getCrunchUpdatesAfterDelay(viewMode: ViewMode): Effect = Effect(Future(GetCrunchStateUpdates(viewMode))).after(requestFrequency(viewMode))

  def requestFrequency(viewMode: ViewMode): FiniteDuration = viewMode match {
    case ViewLive => liveRequestFrequency
    case _ => forecastRequestFrequency
  }
}
