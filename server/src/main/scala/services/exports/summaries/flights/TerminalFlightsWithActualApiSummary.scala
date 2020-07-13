package services.exports.summaries.flights

import drt.shared.ApiFlightWithSplits
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.api.Arrival
import services.exports.Exports
import services.exports.summaries.flights.TerminalFlightsSummaryLike.TerminalFlightsSummaryLikeGenerator


case class TerminalFlightsWithActualApiSummary(flights: Seq[ApiFlightWithSplits],
                                               millisToDateOnly: MillisSinceEpoch => String,
                                               millisToHoursAndMinutes: MillisSinceEpoch => String,
                                               pcpPaxFn: Arrival => Int
                                              ) extends TerminalFlightsSummaryLike {

  import TerminalFlightsWithActualApiSummary._

  lazy val actualApiHeadings: Seq[String] = actualApiHeadingsForFlights(flights)

  override lazy val csvHeader: String = standardCsvHeader + "," + actualApiHeadings.mkString(",")

  override def toCsv: String = {
    val uniqueApiFlightWithSplits: Seq[(ApiFlightWithSplits, Set[Arrival])] = uniqueArrivalsWithCodeShares(flights)
    val csvData = uniqueApiFlightWithSplits.sortBy(_._1.apiFlight.pcpTime).map(fws => {
      flightWithSplitsToCsvRow(queueNames, fws._1) ::: actualAPISplitsForFlightInHeadingOrder(fws._1, actualApiHeadings).toList
    })
    asCSV(csvData) + lineEnding
  }

  val arrivalHeadings = "IATA,ICAO,Origin,Gate/Stand,Status,Scheduled Date,Scheduled Time,Est Arrival,Act Arrival,Est Chox,Act Chox,Est PCP,Total Pax"
  lazy val standardCsvHeader: String =
    arrivalHeadings + ",PCP Pax," +
      headingsForSplitSource(queueNames, "API") + "," +
      headingsForSplitSource(queueNames, "Historical") + "," +
      headingsForSplitSource(queueNames, "Terminal Average")
}

object TerminalFlightsWithActualApiSummary {
  def actualApiHeadingsForFlights(flights: Seq[ApiFlightWithSplits]): Seq[String] = Seq(
    "API Actual - B5JSSK to Desk",
    "API Actual - B5JSSK to eGates",
    "API Actual - EEA (Machine Readable)",
    "API Actual - EEA (Non Machine Readable)",
    "API Actual - Fast Track (Non Visa)",
    "API Actual - Fast Track (Visa)",
    "API Actual - Non EEA (Non Visa)",
    "API Actual - Non EEA (Visa)",
    "API Actual - Transfer",
    "API Actual - eGates"
  )

  def actualAPISplitsForFlightInHeadingOrder(flight: ApiFlightWithSplits, headings: Seq[String]): Seq[Double] =
    headings.map(h => Exports.actualAPISplitsAndHeadingsFromFlight(flight).toMap.getOrElse(h, 0.0))
      .map(n => Math.round(n).toDouble)

  val generator: TerminalFlightsSummaryLikeGenerator =
    (flights: Seq[ApiFlightWithSplits],
     millisToDateOnly: MillisSinceEpoch => String,
     millisToHoursAndMinutes: MillisSinceEpoch => String,
     pcpPaxFn: Arrival => Int) => TerminalFlightsWithActualApiSummary(flights, millisToDateOnly, millisToHoursAndMinutes, pcpPaxFn)
}
