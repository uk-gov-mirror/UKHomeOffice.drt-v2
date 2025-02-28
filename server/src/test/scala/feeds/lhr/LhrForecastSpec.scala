package feeds.lhr

import drt.server.feeds.lhr.forecast.{LhrForecastArrival, LhrForecastArrivals}
import drt.shared.Terminals.T3
import drt.shared.api.Arrival
import drt.shared.{ArrivalStatus, ForecastFeedSource, Operator, PortCode}
import org.specs2.mutable.Specification
import services.SDate

import scala.io.Source
import scala.util.{Failure, Success}


class LhrForecastSpec extends Specification {

  "Given a line from the CSV of the new LHR forecast feed " +
    "When I ask for a parsed Arrival " +
    "Then I should see an Arrival representing the CSV data " >> {
    val csvContent =
      """Terminal,Arr / Dep,DOW,Scheduled Date,Scheduled Time,Prefix,Flight No,Orig / Dest,Orig / Dest Market,Last / Next,Last / Next Market,Aircraft,Capacity,Total Pax,Transfer Pax,Direct Pax,Transfer Demand,Direct Demand
        |3,A,Thu,2018-02-22,04:45:00,BA,BA 0058,CPT,Africa,CPT,Africa,744,337,333,142,191,131,201""".stripMargin

    val arrivalLines = csvContent.split("\n").drop(1)

    val arrival = LhrForecastArrivals(arrivalLines).head

    val expected = Arrival(Operator = Option(Operator("BA")), Status = ArrivalStatus("Forecast"), Estimated = None, Actual = None,
      EstimatedChox = None, ActualChox = None, Gate = None, Stand = None, MaxPax = Option(337),
      ActPax = Option(333), TranPax = Option(142), RunwayID = None, BaggageReclaimId = None, AirportID = PortCode("LHR"), Terminal = T3,
      rawICAO = "BA0058", rawIATA = "BA0058", Origin = PortCode("CPT"), FeedSources = Set(ForecastFeedSource),
      Scheduled = SDate("2018-02-22T04:45:00").millisSinceEpoch, PcpTime = None)

    arrival === expected
  }

  "Given a forecast feed item with 0 passengers for max, direct and transfer. " +
    "When I ask for a parsed Arrival " +
    "Then I should see 0 in ActPax and TransPax " >> {
    val csvContent =
      """Terminal,Arr / Dep,DOW,Scheduled Date,Scheduled Time,Prefix,Flight No,Orig / Dest,Orig / Dest Market,Last / Next,Last / Next Market,Aircraft,Capacity,Total Pax,Transfer Pax,Direct Pax,Transfer Demand,Direct Demand
        |3,A,Thu,2018-02-22,04:45:00,BA,BA 0058,CPT,Africa,CPT,Africa,744,0,0,0,191,131,201""".stripMargin

    val arrivalLines = csvContent.split("\n").drop(1)

    val arrival: Arrival = LhrForecastArrivals(arrivalLines).head
    val actMaxTran = (arrival.ActPax, arrival.MaxPax, arrival.TranPax)

    val expected = (Some(0),Some(0),Some(0) )

    actMaxTran === expected
  }

  "Given an entire CSV " +
    "When I ask for the Arrivals " +
    "Then I should see all the valid lines from the CSV as Arrivals" >> {
    skipped("exploratory")
    val filename = "/tmp/lhr-forecast.csv"
    val fileSource = Source.fromFile(filename)
    val arrivalTries = fileSource.getLines.toSeq.drop(1).map(LhrForecastArrival(_))
    val totalEntries = arrivalTries.length
    val arrivals = arrivalTries
      .filter {
        case Success(_) => true
        case Failure(t) =>
          println(s"failed: $t")
          false
      }
      .collect {
        case Success(a) => a
      }

    fileSource.close()

    val totalArrivals = arrivals.length

    println(s"parsed $totalArrivals from $totalEntries")

    true
  }
}
