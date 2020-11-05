package services.crunch

import actors.AggregatedArrivalsActor
import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.Timeout
import controllers.ArrivalGenerator
import drt.shared.CrunchApi.{CrunchMinute, MillisSinceEpoch, StaffMinute}
import drt.shared.FlightsApi.Flights
import drt.shared.SplitRatiosNs.SplitSources
import drt.shared.Terminals.{T1, Terminal}
import drt.shared._
import drt.shared.api.Arrival
import org.specs2.specification.BeforeEach
import server.feeds.ArrivalsFeedSuccess
import services.SDate
import slick.jdbc.SQLActionBuilder
import slick.jdbc.SetParameter.SetUnit
import slickdb.{AggregatedArrival, AggregatedArrivals, ArrivalTable, ArrivalTableLike}
import test.feeds.test.GetArrivals

import scala.collection.immutable.{List, Seq, SortedMap}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try


object UpdateHandled

object RemovalHandled

class TestAggregatedArrivalsActor(arrivalTable: ArrivalTableLike, probe: ActorRef) extends AggregatedArrivalsActor(arrivalTable) {
  def testReceive: Receive = {
    case GetArrivals => sender() ! arrivalTable.selectAll
  }

  override def receive: Receive = testReceive orElse super.receive

  override def handleRemoval(number: Int, terminal: Terminal, scheduled: MillisSinceEpoch): Unit = {
    super.handleRemoval(number, terminal, scheduled)
    probe ! RemovalHandled
  }

  override def handleUpdate(flightUpdate: Arrival): Unit = {
    super.handleUpdate(flightUpdate)
    probe ! UpdateHandled
  }
}

class AggregatedArrivalsSpec extends CrunchTestLike with BeforeEach {
  override def before: Any = {
    clearDatabase()
  }

  val table: ArrivalTable = ArrivalTable(defaultAirportConfig.portCode, H2Tables)

  def clearDatabase(): Unit = {
    Try(dropTables())
    createTables()
  }

  def createTables(): Unit = {
    H2Tables.schema.createStatements.toList.foreach { query =>
      Await.ready(table.db.run(SQLActionBuilder(List(query), SetUnit).asUpdate), 10 seconds)
    }
  }

  def dropTables(): Unit = {
    H2Tables.schema.dropStatements.toList.reverse.foreach { query =>
      Await.ready(table.db.run(SQLActionBuilder(List(query), SetUnit).asUpdate), 10 seconds)
    }
  }

  def aggregatedArrivalsTestActor(actorProbe: ActorRef, arrivalTable: ArrivalTableLike): ActorRef = {
    system.actorOf(Props(classOf[TestAggregatedArrivalsActor], arrivalTable, actorProbe), name = "aggregated-arrivals-actor")
  }

//  "Given a live arrival " +
//    "When I inspect the message received by the aggregated arrivals actor " +
//    "Then I should see no removals and one update " >> {
//
//    val scheduled = "2017-01-01T00:00Z"
//
//    val liveArrival = ArrivalGenerator.arrival(schDt = scheduled, iata = "BA0001", terminal = T1, actPax = Option(21))
//    val liveFlights = Flights(List(liveArrival))
//
//    val testProbe = TestProbe("arrivals-probe")
//
//    val crunch = runCrunchGraph(TestConfig(
//      now = () => SDate(scheduled),
//      maybeAggregatedArrivalsActor = Option(aggregatedArrivalsTestActor(testProbe.ref, table))
//    ))
//
//    offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(liveFlights))
//
//    testProbe.expectMsg(UpdateHandled)
//
//    val arrivalsResult = Await.result(crunch.aggregatedArrivalsActor.ask(GetArrivals)(new Timeout(5 seconds)), 5 seconds) match {
//      case ag: AggregatedArrivals => ag
//    }
//
//    val expected = AggregatedArrival(liveArrival, defaultAirportConfig.portCode.iata)
//
//    arrivalsResult === AggregatedArrivals(Seq(expected))
//  }
//
//  "Given an existing arrival which is due to expire and a new live arrival " +
//    "When I inspect the aggregated arrivals " +
//    "Then I should see both arrivals, ie the expired arrival is not removed because it's in the past " >> {
//
//    val scheduledExpired = "2017-01-05T00:00Z"
//    val scheduled = "2017-01-05T00:01Z"
//
//    val expiredArrival = ArrivalGenerator.arrival(schDt = scheduledExpired, iata = "BA0022", terminal = T1, actPax = Option(21))
//
//    table.insertOrUpdateArrival(expiredArrival)
//
//    val liveArrival = ArrivalGenerator.arrival(schDt = scheduled, iata = "BA0001", terminal = T1, actPax = Option(21))
//    val liveFlights = Flights(List(liveArrival))
//
//    val oldSplits = Splits(Set(ApiPaxTypeAndQueueCount(PaxTypes.VisaNational, Queues.NonEeaDesk, 100, None)), SplitSources.Historical, None, Percentage)
//    val initialFlightsWithSplits = Seq(ApiFlightWithSplits(expiredArrival, Set(oldSplits), None))
//    val initialPortState = PortState(SortedMap[UniqueArrival, ApiFlightWithSplits]() ++ initialFlightsWithSplits.map(f => (f.apiFlight.unique, f)), SortedMap[TQM, CrunchMinute](), SortedMap[TM, StaffMinute]())
//
//    val testProbe = TestProbe("arrivals-probe")
//
//    val crunch = runCrunchGraph(TestConfig(
//      initialForecastBaseArrivals = SortedMap[UniqueArrival, Arrival](expiredArrival.unique -> expiredArrival),
//      initialPortState = Option(initialPortState),
//      now = () => SDate(scheduled),
//      expireAfterMillis = 250,
//      maybeAggregatedArrivalsActor = Option(aggregatedArrivalsTestActor(testProbe.ref, table))
//    ))
//
//    offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(liveFlights))
//
//    testProbe.expectMsg(UpdateHandled)
//
//    val arrivalsResult = Await.result(crunch.aggregatedArrivalsActor.ask(GetArrivals)(new Timeout(5 seconds)), 5 seconds) match {
//      case ag: AggregatedArrivals => ag.arrivals.toSet
//    }
//
//    val expected = Set(
//      AggregatedArrival(liveArrival, defaultAirportConfig.portCode.iata),
//      AggregatedArrival(expiredArrival, defaultAirportConfig.portCode.iata)
//    )
//
//    arrivalsResult === expected
//  }
//
//  "Given an existing future base arrival followed by an empty list of base arrivals " +
//    "When I inspect the aggregated arrivals " +
//    "Then I should see no arrivals" >> {
//
//    val scheduledDescheduled = "2017-01-10T00:00Z"
//    val scheduled = "2017-01-05T00:00Z"
//
//    val descheduledArrival = ArrivalGenerator.arrival(schDt = scheduledDescheduled, iata = "BA0022", terminal = T1, actPax = Option(21))
//
//    table.insertOrUpdateArrival(descheduledArrival)
//
//    val oldSplits = Splits(Set(ApiPaxTypeAndQueueCount(PaxTypes.VisaNational, Queues.NonEeaDesk, 100, None)), SplitSources.Historical, None, Percentage)
//    val initialFlightsWithSplits = Seq(ApiFlightWithSplits(descheduledArrival, Set(oldSplits), None))
//    val initialPortState = PortState(SortedMap[UniqueArrival, ApiFlightWithSplits]() ++ initialFlightsWithSplits.map(f => (f.apiFlight.unique, f)), SortedMap[TQM, CrunchMinute](), SortedMap[TM, StaffMinute]())
//
//    val testProbe = TestProbe("arrivals-probe")
//
//    val crunch = runCrunchGraph(TestConfig(
//      initialForecastBaseArrivals = SortedMap[UniqueArrival, Arrival](descheduledArrival.unique -> descheduledArrival),
//      initialPortState = Option(initialPortState),
//      now = () => SDate(scheduled),
//      expireAfterMillis = 250,
//      maybeAggregatedArrivalsActor = Option(aggregatedArrivalsTestActor(testProbe.ref, table))
//    ))
//
//    offerAndWait(crunch.baseArrivalsInput, ArrivalsFeedSuccess(Flights(List())))
//
//    testProbe.expectMsg(RemovalHandled)
//
//    val arrivalsResult = Await.result(crunch.aggregatedArrivalsActor.ask(GetArrivals)(new Timeout(5 seconds)), 5 seconds) match {
//      case ag: AggregatedArrivals => ag.arrivals.toSet
//    }
//
//    val expected = Set()
//
//    arrivalsResult === expected
//  }

}

