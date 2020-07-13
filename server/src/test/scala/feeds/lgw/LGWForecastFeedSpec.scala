package feeds.lgw

import com.box.sdk.{BoxConfig, BoxDeveloperEditionAPIConnection}
import drt.server.feeds.lgw.LGWForecastFeed
import drt.shared.Terminals.S
import drt.shared.api.Arrival
import drt.shared.{ForecastFeedSource, PortCode}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import services.SDate

import scala.util.{Failure, Success, Try}

class LGWForecastFeedSpec extends Specification with Mockito {

  trait Context extends Scope {
    val filePath: String = getClass.getClassLoader.getResource("box-config.json").getPath
    val userId = ""
    val ukBfGalForecastFolderId = ""
  }

  trait ExampleContext extends Context {
    val exampleData: String =
      """Date,Flight Number,POA Forecast Version,Seats,Aircraft Type,Terminal,ArrDep,Orig Dest,Airport Code,Scheduled Time,POA Pax,Transfer Pax,CSA Pax,Prefix,Time,Hour,Int/Dom,Date/Time
        |19-May-18,3O0101,POA FCST 17-05-18,174,,South,Arrival,"Tangier (Ibn Batuta), Morocco",TNG,1105,134,0,134,3O,11:05,11,INTL,19/05/2018 11:05
        |,,,,,,,,,,,,,,,,,
        |,,,,,,,,,,,,,,,,,
        |
      """.stripMargin

    val exampleDataWith0Pax: String =
      """Date,Flight Number,POA Forecast Version,Seats,Aircraft Type,Terminal,ArrDep,Orig Dest,Airport Code,Scheduled Time,POA Pax,Transfer Pax,CSA Pax,Prefix,Time,Hour,Int/Dom,Date/Time
        |19-May-18,3O0101,POA FCST 17-05-18,0,,South,Arrival,"Tangier (Ibn Batuta), Morocco",TNG,1105,0,0,0,3O,11:05,11,INTL,19/05/2018 11:05
        |,,,,,,,,,,,,,,,,,
        |,,,,,,,,,,,,,,,,,
        |
      """.stripMargin
  }

  import drt.server.feeds.Implicits._

  "The LGW Forecast Feed" should {

    "parse the arrivals given a CSV" in new ExampleContext {
      val feed: LGWForecastFeed = new LGWForecastFeed(filePath, userId, ukBfGalForecastFolderId) {
        override def getBoxConfig: BoxConfig = mock[BoxConfig]
        override def getApiConnection: Try[BoxDeveloperEditionAPIConnection] = Try(mock[BoxDeveloperEditionAPIConnection])
      }

      val arrivals: List[Arrival] = feed.getArrivalsFromData("aFile.csv", exampleData)
      arrivals.length mustEqual 1
      arrivals.head mustEqual Arrival(
        operator = None,
        status = "Port Forecast",
        estimated = None,
        actual = None,
        estimatedChox = None,
        actualChox = None,
        gate = None,
        stand = None,
        maxPax = Option(174),
        actPax = Option(134),
        tranPax = Some(0),
        runwayID = None,
        baggageReclaimId = None,
        airportID = PortCode("LGW"),
        terminal = S,
        rawICAO = "3O0101",
        rawIATA = "3O0101",
        origin = PortCode("TNG"),
        scheduled = SDate("2018-05-19T10:05:00Z").millisSinceEpoch,
        pcpTime = None,
        feedSources = Set(ForecastFeedSource)
      )
    }

    "Given 0 passengers for Act and Max and Trans should reflect this in the parsed arrival" in new ExampleContext {
      val feed: LGWForecastFeed = new LGWForecastFeed(filePath, userId, ukBfGalForecastFolderId) {
        override def getBoxConfig: BoxConfig = mock[BoxConfig]
        override def getApiConnection: Try[BoxDeveloperEditionAPIConnection] = Try(mock[BoxDeveloperEditionAPIConnection])
      }

      val arrivals: List[Arrival] = feed.getArrivalsFromData("aFile.csv", exampleDataWith0Pax)
      arrivals.length mustEqual 1
      arrivals.head mustEqual Arrival(
        operator = None,
        status = "Port Forecast",
        estimated = None,
        actual = None,
        estimatedChox = None,
        actualChox = None,
        gate = None,
        stand = None,
        maxPax = Option(0),
        actPax = Option(0),
        tranPax = Some(0),
        runwayID = None,
        baggageReclaimId = None,
        airportID = PortCode("LGW"),
        terminal = S,
        rawICAO = "3O0101",
        rawIATA = "3O0101",
        origin = PortCode("TNG"),
        scheduled = SDate("2018-05-19T10:05:00Z").millisSinceEpoch,
        pcpTime = None,
        feedSources = Set(ForecastFeedSource)
      )
    }

    "Can return the exception if we cannot get the latest file" in new Context {
      val expectedError = "an error"
      val feed: LGWForecastFeed = new LGWForecastFeed(filePath, userId, ukBfGalForecastFolderId) {
        override def getBoxConfig: BoxConfig = mock[BoxConfig]
        override def getApiConnection: Try[BoxDeveloperEditionAPIConnection] = Try(throw new Exception(expectedError))
      }

      feed.getArrivals must beLike {
        case Failure(e) => e.getMessage mustEqual expectedError
      }
    }

    "Can parse the arrivals in the latest file" in new Context {
      skipped("exploratory test for the LGW forecast feed")
      val feed = new LGWForecastFeed(filePath, userId, ukBfGalForecastFolderId)

      val Success(arrivals: List[Arrival]) = feed.getArrivals

      arrivals.foreach(println)

      println(s"Got ${arrivals.size} arrivals.")

      arrivals.length mustNotEqual 0

    }.pendingUntilFixed("This is not a test")
  }
}
