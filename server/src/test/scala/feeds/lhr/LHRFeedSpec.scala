package feeds.lhr

import akka.NotUsed
import akka.pattern.pipe
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.testkit.TestProbe
import drt.server.feeds.lhr.{LHRFlightFeed, LHRLiveFlight}
import drt.shared.Terminals.{T1, T4}
import drt.shared.api.Arrival
import drt.shared.{ArrivalStatus, LiveFeedSource, Operator, PortCode}
import org.apache.commons.csv.{CSVFormat, CSVParser, CSVRecord}
import services.SDate
import services.crunch.CrunchTestLike

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class LHRFeedSpec extends CrunchTestLike {

  "lhrCsvToApiFlights" should {
    "Produce an Arrival source with one flight based on a line from the LHR csv" in {
      val csvString =
        """|Term","Flight No","Operator","From","Airport name","Scheduled","Estimated","Touchdown","Est Chocks","Act Chocks","Stand","Max pax","Act Pax","Conn pax"
           |"4","QR005","Qatar Airways","DOH","Doha","22:00 09/03/2017","21:32 09/03/2017","21:33 09/03/2017","21:43 09/03/2017","21:45 09/03/2017","10","795","142","1""""
          .stripMargin

      val csvGetters: Iterator[(Int) => String] = LHRFlightFeed.csvParserAsIteratorOfColumnGetter(csvString)
      val lhrFeed = LHRFlightFeed(csvGetters)

      val probe = TestProbe()

      val flightsSource: Source[List[Arrival], NotUsed] = Source(List(lhrFeed.copiedToApiFlights))

      val futureFlightsSeq: Future[Seq[List[Arrival]]] = flightsSource.runWith(Sink.seq).pipeTo(probe.ref)

      val flights = Await.result(futureFlightsSeq, 3 seconds).asInstanceOf[Vector[Arrival]]

      flights.toList === List(
        List(
          Arrival(
            operator = Option(Operator("Qatar Airways")),
            status = ArrivalStatus("UNK"),
            estimated = Option(SDate("2017-03-09T21:32:00.000Z").millisSinceEpoch),
            actual = Option(SDate("2017-03-09T21:33:00.000Z").millisSinceEpoch),
            estimatedChox = Option(SDate("2017-03-09T21:43:00.000Z").millisSinceEpoch),
            actualChox = Option(SDate("2017-03-09T21:45:00.000Z").millisSinceEpoch),
            gate = None, stand = Option("10"), maxPax = Option(795), actPax = Option(142), tranPax = Option(1), runwayID = None, baggageReclaimId = None,
            airportID = PortCode("LHR"), terminal = T4, rawICAO = "QR005", rawIATA = "QR005", origin = PortCode("DOH"),
            scheduled = SDate("2017-03-09T22:00:00.000Z").millisSinceEpoch,
            pcpTime = Option(SDate("2017-03-09T22:04:00.000Z").millisSinceEpoch), feedSources = Set(LiveFeedSource)
          )
        )
      )
    }

    "Should accept 0 as a valid Pax value for all Pax fields" in {
      val csvString =
        """|Term","Flight No","Operator","From","Airport name","Scheduled","Estimated","Touchdown","Est Chocks","Act Chocks","Stand","Max pax","Act Pax","Conn pax"
           |"4","QR005","Qatar Airways","DOH","Doha","22:00 09/03/2017","21:32 09/03/2017","21:33 09/03/2017","21:43 09/03/2017","21:45 09/03/2017","10","0","0","0""""
          .stripMargin

      implicit val materializer: ActorMaterializer = ActorMaterializer()
      val csvGetters: Iterator[(Int) => String] = LHRFlightFeed.csvParserAsIteratorOfColumnGetter(csvString)
      val lhrFeed = LHRFlightFeed(csvGetters)

      val probe = TestProbe()

      val flightsSource: Source[List[Arrival], NotUsed] = Source(List(lhrFeed.copiedToApiFlights))

      val futureFlightsSeq: Future[Seq[List[Arrival]]] = flightsSource.runWith(Sink.seq).pipeTo(probe.ref)

      val flights = Await.result(futureFlightsSeq, 3 seconds).asInstanceOf[Vector[Arrival]]

      flights.toList === List(
        List(
          Arrival(
            operator = Option(Operator("Qatar Airways")),
            status = ArrivalStatus("UNK"),
            estimated = Option(SDate("2017-03-09T21:32:00.000Z").millisSinceEpoch),
            actual = Option(SDate("2017-03-09T21:33:00.000Z").millisSinceEpoch),
            estimatedChox = Option(SDate("2017-03-09T21:43:00.000Z").millisSinceEpoch),
            actualChox = Option(SDate("2017-03-09T21:45:00.000Z").millisSinceEpoch),
            gate = None,
            stand = Option("10"),
            maxPax = Option(0),
            actPax = Option(0),
            tranPax = Option(0),
            runwayID = None,
            baggageReclaimId = None,
            airportID = PortCode("LHR"),
            terminal = T4,
            rawICAO = "QR005",
            rawIATA = "QR005",
            origin = PortCode("DOH"),
            scheduled = SDate("2017-03-09T22:00:00.000Z").millisSinceEpoch,
            pcpTime = Option(SDate("2017-03-09T22:04:00.000Z").millisSinceEpoch),
            feedSources = Set(LiveFeedSource)
          )
        )
      )
    }

    "Produce an Arrival source with one flight based on a line with missing values from the LHR csv" in {
      val csvString =
        """|Term","Flight No","Operator","From","Airport name","Scheduled","Estimated","Touchdown","Est Chocks","Act Chocks","Stand","Max pax","Act Pax","Conn pax"
           |"4","KL1033","KLM Royal Dutch Airlines","AMS","Amsterdam","20:50 09/03/2017","20:50 09/03/2017","","","","","","","""""
          .stripMargin

      implicit val materializer: ActorMaterializer = ActorMaterializer()

      val csv: CSVParser = CSVParser.parse(csvString, CSVFormat.DEFAULT)
      val csvGetters: Iterator[(Int) => String] = csv.iterator().asScala.map((l: CSVRecord) => (i: Int) => l.get(i))
      val lhrFeed = LHRFlightFeed(csvGetters)


      val probe = TestProbe()
      val flightsSource: Source[List[Arrival], NotUsed] = Source(List(lhrFeed.copiedToApiFlights))
      val futureFlightsSeq: Future[Seq[List[Arrival]]] = flightsSource.runWith(Sink.seq).pipeTo(probe.ref)

      val flights = Await.result(futureFlightsSeq, 3 seconds)

      flights match {
        case Vector(List(_: Arrival)) =>
          true
        case _ =>
          false
      }
    }

    "should consistently return the same flightid for the same flight" in {
      val flightV1 = LHRLiveFlight(T1, "SA123", "SAA", "JHB", "LHR", org.joda.time.DateTime.parse("2017-01-01T20:00:00z"), None, None, None, None, None, None, None, None)
      val flightV2 = LHRLiveFlight(T1, "SA123", "SAA", "JHB", "LHR", org.joda.time.DateTime.parse("2017-01-01T20:00:00z"), None, None, None, None, None, None, None, None)

      flightV1.flightId() === flightV2.flightId()
    }

    "should not return the same flightid for different flights" in {
      val flightv1 = LHRLiveFlight(T1, "SA324", "SAA", "JHB", "LHR", org.joda.time.DateTime.parse("2017-01-01T20:00:00z"), None, None, None, None, None, None, None, None)
      val flightv2 = LHRLiveFlight(T1, "SA123", "SAA", "JHB", "LHR", org.joda.time.DateTime.parse("2017-01-01T20:00:00z"), None, None, None, None, None, None, None, None)

      flightv1.flightId() !== flightv2.flightId()
    }
  }
}

