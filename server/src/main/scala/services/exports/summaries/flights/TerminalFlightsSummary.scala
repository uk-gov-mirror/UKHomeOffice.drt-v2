package services.exports.summaries.flights

import drt.shared.ApiFlightWithSplits
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.api.Arrival
import services.exports.summaries.flights.TerminalFlightsSummary._
import services.exports.summaries.flights.TerminalFlightsSummaryLike.TerminalFlightsSummaryLikeGenerator

case class TerminalFlightsSummary(flights: Seq[ApiFlightWithSplits],
                                  millisToDateOnly: MillisSinceEpoch => String,
                                  millisToHoursAndMinutes: MillisSinceEpoch => String,
                                  pcpPaxFn: Arrival => Int
                                 ) extends TerminalFlightsSummaryLike {

  override lazy val csvHeader: String =
    rawArrivalHeadings + ",PCP Pax," +
      headingsForSplitSource(queueNames, "API") + "," +
      headingsForSplitSource(queueNames, "Historical") + "," +
      headingsForSplitSource(queueNames, "Terminal Average")

  override def toCsv: String = {
    val uniqueApiFlightWithSplits: Seq[(ApiFlightWithSplits, Set[Arrival])] = uniqueArrivalsWithCodeShares(flights)
    val csvData = uniqueApiFlightWithSplits.sortBy(_._1.apiFlight.pcpTime).map(fws =>
      flightWithSplitsToCsvRow(queueNames, fws._1)
    )
    asCSV(csvData) + lineEnding
  }
}

object TerminalFlightsSummary {
  def empty(millisToLocalIsoDateOnly: MillisSinceEpoch => String,
            millisToLocalHoursAndMinutes: MillisSinceEpoch => String,
            pcpPaxFn: Arrival => Int): TerminalFlightsSummary = TerminalFlightsSummary(Seq(), millisToLocalIsoDateOnly, millisToLocalHoursAndMinutes, pcpPaxFn)


  val rawArrivalHeadings = "IATA,ICAO,Origin,Gate/Stand,Status,Scheduled Date,Scheduled Time,Est Arrival,Act Arrival,Est Chox,Act Chox,Est PCP,Total Pax"
  val rawArrivalHeadingsWithTransfer: String = rawArrivalHeadings + ",Transfer Pax"

  def arrivalAsRawCsvValues(arrival: Arrival, millisToDateOnly: MillisSinceEpoch => String,
                            millisToHoursAndMinutes: MillisSinceEpoch => String): List[String] = {
    List(arrival.flightCode,
      arrival.flightCode,
      arrival.origin.toString,
      arrival.gate.getOrElse("") + "/" + arrival.stand.getOrElse(""),
      arrival.status.description,
      millisToDateOnly(arrival.scheduled),
      millisToHoursAndMinutes(arrival.scheduled),
      arrival.sstimated.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.actual.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.estimatedChox.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.actualChox.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.pcpTime.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.actPax.getOrElse("").toString)
  }

  def arrivalAsRawCsvValuesWithTransfer(arrival: Arrival, millisToDateOnly: MillisSinceEpoch => String,
                                        millisToHoursAndMinutes: MillisSinceEpoch => String): List[String] =
    arrivalAsRawCsvValues(arrival, millisToDateOnly, millisToHoursAndMinutes) :+ arrival.tranPax.getOrElse(0).toString

  val generator: TerminalFlightsSummaryLikeGenerator =
    (flights: Seq[ApiFlightWithSplits],
     millisToDateOnly: MillisSinceEpoch => String,
     millisToHoursAndMinutes: MillisSinceEpoch => String,
     pcpPaxFn: Arrival => Int) => TerminalFlightsSummary(flights, millisToDateOnly, millisToHoursAndMinutes, pcpPaxFn)

}
