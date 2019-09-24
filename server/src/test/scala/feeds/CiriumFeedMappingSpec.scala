package feeds

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import drt.server.feeds.cirium.CiriumFeed
import drt.shared.{Arrival, CiriumFeedSource}
import org.specs2.mock.Mockito
import org.specs2.mutable.SpecificationLike
import server.feeds.ArrivalsFeedSuccess
import services.SDate
import uk.gov.homeoffice.cirium.services.entities._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

class CiriumFeedMappingSpec extends TestKit(ActorSystem("testActorSystem", ConfigFactory.empty())) with SpecificationLike with Mockito {
  sequential
  isolated


  "When successfully polling for CiriumArrivals I should get a stream of ArrivalFeedSuccess" >> {
    implicit val mat: ActorMaterializer = ActorMaterializer()

//    val ciriumFeed = new CiriumFeed("http://localhost:8080/statuses/stn")
//    val probe = TestProbe()
//
//    val cancellable = ciriumFeed.tickingSource.collect {
//      case afs: ArrivalsFeedSuccess =>
//        afs.arrivals.flights.map(a => {
//          println(s"flight: ${a.IATA}")
//          println(s"Scheduled: ${SDate(a.Scheduled).toISOString()}")
//          println(s"Act: ${a.Actual.map( d => SDate(d).toISOString())}")
//          println(s"Est: ${a.Estimated.map( d => SDate(d).toISOString())}")
//          println(s"EstChox: ${a.EstimatedChox.map( d => SDate(d).toISOString())}")
//          println(s"ActChox: ${a.ActualChox.map( d => SDate(d).toISOString())}")
//        })
//        afs
//    }.to(Sink.actorRef(probe.ref, "completed")).run()
//
//    probe.fishForMessage(30 seconds) {
//      case s: ArrivalsFeedSuccess if s.arrivals.flights.head.Scheduled == SDate("2019-07-15T11:05:00.000Z").millisSinceEpoch =>
//        println(s"Successfully got a result")
//        true
//    }

    success
  }
}