package feeds.lgw

import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.testkit.TestProbe
import drt.server.feeds.lgw.{LGWAzureClient, LGWFeed, ResponseToArrivals}
import drt.shared.Terminals.N
import drt.shared.api.Arrival
import drt.shared.{LiveFeedSource, PortCode}
import org.specs2.mock.Mockito
import server.feeds.{ArrivalsFeedFailure, ArrivalsFeedSuccess}
import services.SDate
import services.crunch.CrunchTestLike
import scala.concurrent.duration._

import scala.collection.immutable.Seq
import scala.io.Source

class LGWFeedSpec extends CrunchTestLike with Mockito {
  sequential
  isolated

  import drt.server.feeds.Implicits._

  "Can convert response XML into an Arrival" in  {

    val xml: String = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("lgw.xml")).mkString

    val arrivals: Seq[Arrival] = ResponseToArrivals(xml).getArrivals

    arrivals.size mustEqual 1
    arrivals.head mustEqual Arrival(
      operator = None,
      status = "Landed",
      estimated = Some(SDate("2018-06-03T19:28:00Z").millisSinceEpoch),
      actual =  Some(SDate("2018-06-03T19:30:00Z").millisSinceEpoch),
      estimatedChox =  Some(SDate("2018-06-03T19:37:00Z").millisSinceEpoch),
      actualChox =  Some(SDate("2018-06-03T19:36:00Z").millisSinceEpoch),
      gate = None,
      stand = None,
      maxPax = Some(308),
      actPax = Some(120),
      tranPax = None,
      runwayID = Some("08R"),
      baggageReclaimId = None,
      airportID = PortCode("LGW"),
      terminal = N,
      rawICAO = "VIR808",
      rawIATA = "VS808",
      origin = PortCode("LHR"),
      feedSources = Set(LiveFeedSource),
      scheduled = SDate("2018-06-03T19:50:00Z").millisSinceEpoch, pcpTime = None)


  }

  "Given a feed item with 0 pax in act and max then I should see that reflected in the arrival" in  {

    val xml: String = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("lgwWith0Pax.xml")).mkString

    val arrivals: Seq[Arrival] = ResponseToArrivals(xml).getArrivals

    arrivals.size mustEqual 1
    arrivals.head mustEqual Arrival(
      operator = None,
      status = "Landed",
      estimated = Some(SDate("2018-06-03T19:28:00Z").millisSinceEpoch),
      actual =  Some(SDate("2018-06-03T19:30:00Z").millisSinceEpoch),
      estimatedChox =  Some(SDate("2018-06-03T19:37:00Z").millisSinceEpoch),
      actualChox =  Some(SDate("2018-06-03T19:36:00Z").millisSinceEpoch),
      gate = None,
      stand = None,
      maxPax = Some(0),
      actPax = Some(0),
      tranPax = None,
      runwayID = Some("08R"),
      baggageReclaimId = None,
      airportID = PortCode("LGW"),
      terminal = N,
      rawICAO = "VIR808",
      rawIATA = "VS808",
      origin = PortCode("LHR"),
      feedSources = Set(LiveFeedSource),
      scheduled = SDate("2018-06-03T19:50:00Z").millisSinceEpoch, pcpTime = None)


  }

  "An empty response returns an empty list of arrivals" in  {
    val xml: String = ""

    val arrivals: Seq[Arrival] = ResponseToArrivals(xml).getArrivals

    arrivals mustEqual List()

  }
  "A bad response returns an empty list of arrivals" in  {
    val xml: String = "<thing>some not valid xml</thing>"

    val arrivals: Seq[Arrival] = ResponseToArrivals(xml).getArrivals

    arrivals mustEqual List()

  }

  "Exploratory test" >> {
    skipped("Exploratory")
    val lgwNamespace = ""
    val lgwSasToKey = ""
    val lgwServiceBusUri = ""
    val azureClient = LGWAzureClient(LGWFeed.serviceBusClient(lgwNamespace, lgwSasToKey, lgwServiceBusUri))

    val probe = TestProbe()
    LGWFeed(azureClient)(system).source().map{
      case s: ArrivalsFeedSuccess =>
        println(s.arrivals)
      case f: ArrivalsFeedFailure =>
        println(f.responseMessage)
    }.runWith(Sink.foreach(probe.ref ! _ ))

    probe.fishForMessage(5 minutes){
      case x => println(x)
        false
    }

    true
  }

}
