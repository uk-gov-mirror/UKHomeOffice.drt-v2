package services.exports

import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.PaxTypes.UndefinedPaxType
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.graphstages.Crunch


object Exports {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def millisToLocalIsoDateOnly: MillisSinceEpoch => String = (millis: MillisSinceEpoch) => SDate.millisToLocalIsoDateOnly(Crunch.europeLondonTimeZone)(millis)

  def millisToLocalHoursAndMinutes: MillisSinceEpoch => String = (millis: MillisSinceEpoch) => SDate.millisToLocalHoursAndMinutes(Crunch.europeLondonTimeZone)(millis)

  def millisToUtcIsoDateOnly: MillisSinceEpoch => String = (millis: MillisSinceEpoch) => SDate(millis).toISODateOnly

  def millisToUtcHoursAndMinutes: MillisSinceEpoch => String = (millis: MillisSinceEpoch) => SDate(millis).toHoursAndMinutes

  def actualAPISplitsAndHeadingsFromFlight(flightWithSplits: ApiFlightWithSplits): Set[(String, Double)] = flightWithSplits
    .splits
    .collect {
      case s: Splits if s.source == SplitRatiosNs.SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages =>
        s.splits
          .map(s =>
            (PaxTypesAndQueues.displayName.get(s.paxTypeAndQueue), s.paxCount))
          .collect {
            case (Some(displayName), paxCount) => (s"API Actual - $displayName", paxCount)
          }
    }
    .flatten

}
