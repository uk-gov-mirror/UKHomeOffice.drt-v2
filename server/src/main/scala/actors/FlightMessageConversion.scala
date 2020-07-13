package actors

import actors.PortStateMessageConversion.splitMessageToApiSplits
import actors.restore.RestorerWithLegacy
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.Arrival
import org.slf4j.{Logger, LoggerFactory}
import server.protobuf.messages.CrunchState.{FlightWithSplitsMessage, FlightsWithSplitsMessage, PaxTypeAndQueueCountMessage, SplitMessage}
import server.protobuf.messages.FlightsMessage.{FeedStatusMessage, FeedStatusesMessage, FlightMessage, FlightStateSnapshotMessage}
import services.SDate

import scala.util.{Success, Try}

object FlightMessageConversion {
  val log: Logger = LoggerFactory.getLogger(getClass.toString)

  def arrivalsStateToSnapshotMessage(state: ArrivalsState): FlightStateSnapshotMessage = {
    val maybeStatusMessages = state.maybeSourceStatuses.flatMap(feedStatuses => feedStatusesToMessage(feedStatuses.feedStatuses))

    FlightStateSnapshotMessage(
      state.arrivals.values.map(apiFlightToFlightMessage).toSeq,
      maybeStatusMessages
      )
  }

  def feedStatusesToMessage(statuses: FeedStatuses): Option[FeedStatusesMessage] = {
    val statusMessages = statuses.statuses.map(feedStatusToMessage)

    Option(FeedStatusesMessage(statusMessages, statuses.lastSuccessAt, statuses.lastFailureAt, statuses.lastUpdatesAt))
  }

  def feedStatusToMessage(feedStatus: FeedStatus): FeedStatusMessage = feedStatus match {
    case s: FeedStatusSuccess => FeedStatusMessage(Option(s.date), Option(s.updateCount), None)
    case s: FeedStatusFailure => FeedStatusMessage(Option(s.date), None, Option(s.message))
  }

  def restoreArrivalsFromSnapshot(restorer: RestorerWithLegacy[Int, UniqueArrival, Arrival],
                                  snMessage: FlightStateSnapshotMessage): Unit = {
    restorer.update(snMessage.flightMessages.map(flightMessageToApiFlight))
  }

  def feedStatusesFromSnapshotMessage(snMessage: FlightStateSnapshotMessage): Option[FeedStatuses] = {
    snMessage.statuses.map(feedStatusesFromFeedStatusesMessage)
  }

  def feedStatusesFromFeedStatusesMessage(message: FeedStatusesMessage): FeedStatuses = FeedStatuses(
    statuses = message.statuses.map(feedStatusFromFeedStatusMessage).toList,
    lastSuccessAt = message.lastSuccessAt,
    lastFailureAt = message.lastFailureAt,
    lastUpdatesAt = message.lastUpdatesAt
    )

  def feedStatusFromFeedStatusMessage(message: FeedStatusMessage): FeedStatus = {
    if (message.updates.isDefined)
      FeedStatusSuccess(message.date.getOrElse(0L), message.updates.getOrElse(0))
    else
      FeedStatusFailure(message.date.getOrElse(0L), message.message.getOrElse("n/a"))
  }

  def flightWithSplitsToMessage(f: ApiFlightWithSplits): FlightWithSplitsMessage = {
    FlightWithSplitsMessage(
      Option(FlightMessageConversion.apiFlightToFlightMessage(f.apiFlight)),
      f.splits.map(apiSplitsToMessage).toList,
      lastUpdated = f.lastUpdated)
  }

  def flightWithSplitsFromMessage(fm: FlightWithSplitsMessage): ApiFlightWithSplits = ApiFlightWithSplits(
    FlightMessageConversion.flightMessageToApiFlight(fm.flight.get),
    fm.splits.map(sm => splitMessageToApiSplits(sm)).toSet,
    lastUpdated = fm.lastUpdated
    )

  def apiSplitsToMessage(s: Splits): SplitMessage = {
    SplitMessage(
      paxTypeAndQueueCount = s.splits.map(paxTypeAndQueueCountToMessage).toList,
      source = Option(s.source.toString),
      eventType = s.maybeEventType.map(_.toString),
      style = Option(s.splitStyle.name)
      )
  }

  def paxTypeAndQueueCountToMessage(ptqc: ApiPaxTypeAndQueueCount): PaxTypeAndQueueCountMessage = {
    PaxTypeAndQueueCountMessage(
      Option(ptqc.passengerType.name),
      Option(ptqc.queueType.toString),
      Option(ptqc.paxCount)
      )
  }

  def apiFlightToFlightMessage(apiFlight: Arrival): FlightMessage = {
    FlightMessage(
      operator = apiFlight.operator.map(_.code),
      gate = apiFlight.gate,
      stand = apiFlight.stand,
      status = Option(apiFlight.status.description),
      maxPax = apiFlight.maxPax,
      actPax = apiFlight.actPax,
      tranPax = apiFlight.tranPax,
      runwayID = apiFlight.runwayID,
      baggageReclaimId = apiFlight.baggageReclaimId,
      airportID = Option(apiFlight.airportID.iata),
      terminal = Option(apiFlight.terminal.toString),
      iCAO = Option(apiFlight.flightCode),
      iATA = Option(apiFlight.flightCode),
      origin = Option(apiFlight.origin.toString),
      pcpTime = apiFlight.pcpTime.filter(_ != 0),
      feedSources = apiFlight.feedSources.map(_.toString).toSeq,
      scheduled = Option(apiFlight.scheduled).filter(_ != 0),
      estimated = apiFlight.sstimated.filter(_ != 0),
      touchdown = apiFlight.actual.filter(_ != 0),
      estimatedChox = apiFlight.estimatedChox.filter(_ != 0),
      actualChox = apiFlight.actualChox.filter(_ != 0),
      carrierScheduled = apiFlight.carrierScheduled,
      apiPax = apiFlight.apiPax
      )
  }

  def millisOptionFromArrivalDateString(datetime: String): Option[Long] = datetime match {
    case "" => None
    case _ =>
      Try {
        SDate.parseString(datetime)
      } match {
        case Success(MilliDate(millis)) => Some(millis)
        case _ => None
      }
  }

  def flightMessageToApiFlight(flightMessage: FlightMessage): Arrival = {
    Arrival(
      operator = flightMessage.operator.map(Operator),
      status = ArrivalStatus(flightMessage.status.getOrElse("")),
      estimated = flightMessage.estimated,
      actual = flightMessage.touchdown,
      estimatedChox = flightMessage.estimatedChox,
      actualChox = flightMessage.actualChox,
      gate = flightMessage.gate,
      stand = flightMessage.stand,
      maxPax = flightMessage.maxPax,
      actPax = flightMessage.actPax,
      tranPax = flightMessage.tranPax,
      runwayID = flightMessage.runwayID,
      baggageReclaimId = flightMessage.baggageReclaimId,
      airportID = PortCode(flightMessage.airportID.getOrElse("")),
      terminal = Terminal(flightMessage.terminal.getOrElse("")),
      rawICAO = flightMessage.iCAO.getOrElse(""),
      rawIATA = flightMessage.iATA.getOrElse(""),
      origin = PortCode(flightMessage.origin.getOrElse("")),
      pcpTime = flightMessage.pcpTime,
      scheduled = flightMessage.scheduled.getOrElse(0L),
      feedSources = flightMessage.feedSources.flatMap(FeedSource(_)).toSet,
      carrierScheduled = flightMessage.carrierScheduled,
      apiPax = flightMessage.apiPax
      )
  }

  def apiFlightDateTime(millisOption: Option[Long]): String = millisOption match {
    case Some(millis: Long) => SDate.jodaSDateToIsoString(SDate(millis))
    case _ => ""
  }

  def flightsToMessage(flights: Iterable[ApiFlightWithSplits]): FlightsWithSplitsMessage =
    FlightsWithSplitsMessage(flights.map(FlightMessageConversion.flightWithSplitsToMessage).toSeq)
}
