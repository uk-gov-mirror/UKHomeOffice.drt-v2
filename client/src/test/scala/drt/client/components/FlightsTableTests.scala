package drt.client.components

import diode.data.{Pot, Ready}
import drt.client.services.JSDateConversions.SDate
import drt.client.services.ViewLive
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.Arrival
import japgolly.scalajs.react.component.Scala.{Component, Unmounted}
import japgolly.scalajs.react.vdom.{TagOf, html_<^}
import org.scalajs.dom.html.{Span, TableCell, TableSection}
import utest._

import scala.collection.immutable.{Map, Seq}


object FlightsTableTests extends TestSuite {

  import FlightsWithSplitsTable.ArrivalsTable
  import japgolly.scalajs.react._
  import japgolly.scalajs.react.test._
  import japgolly.scalajs.react.vdom.html_<^._

  val queuesWithoutFastTrack: List[Queue] = Queues.queueOrder.filterNot(q => q == Queues.FastTrack || q == Queues.QueueDesk)

  def tests: Tests = Tests {

    def date(dt: Option[MillisSinceEpoch], className: Option[String] = None): VdomTagOf[TableCell] = className match {
      case Some(cn) => <.td(flightDate(dt.map(millis => SDate(millis).toISOString().replaceFirst(":00.000Z", "")).getOrElse("")), ^.className := cn)
      case _ => <.td(flightDate(dt.map(millis => SDate(millis).toISOString().replaceFirst(":00.000Z", "")).getOrElse("")))
    }

    def flightDate(dt: String): VdomTagOf[Span] = <.span(^.title := dt.replace("T", " "), dt.split("T")(1))

    val testLTNFlight = Arrival(
      operator = Option(Operator("Op")),
      status = ArrivalStatus("scheduled"),
      estimated = Option(SDate("2016-01-01T13:05").millisSinceEpoch),
      actual = Option(SDate("2016-01-01T13:10").millisSinceEpoch),
      estimatedChox = Option(SDate("2016-01-01T13:15").millisSinceEpoch),
      actualChox = Option(SDate("2016-01-01T13:20").millisSinceEpoch),
      gate = Option("10"),
      stand = Option("10A"),
      maxPax = Option(200),
      actPax = Option(150),
      tranPax = Option(10),
      runwayID = Option("1"),
      baggageReclaimId = Option("A"),
      airportID = PortCode("ltn"),
      terminal = Terminal("T2"),
      rawICAO = "BA0001",
      rawIATA = "BAA0001",
      origin = PortCode("JFK"),
      pcpTime = Option(1451655000000L), // 2016-01-01 13:30:00 UTC
      scheduled = SDate("2016-01-01T13:00").millisSinceEpoch,
      feedSources = Set(ApiFeedSource)
    )

    val testFlight = Arrival(
      operator = Option(Operator("Op")),
      status = ArrivalStatus("scheduled"),
      estimated = Option(SDate("2016-01-01T13:05").millisSinceEpoch),
      actual = Option(SDate("2016-01-01T13:10").millisSinceEpoch),
      estimatedChox = Option(SDate("2016-01-01T13:15").millisSinceEpoch),
      actualChox = Option(SDate("2016-01-01T13:20").millisSinceEpoch),
      gate = Option("10"),
      stand = Option("10A"),
      maxPax = Option(200),
      actPax = Option(150),
      tranPax = Option(10),
      runwayID = Option("1"),
      baggageReclaimId = Option("A"),
      airportID = PortCode("LHR"),
      terminal = Terminal("T2"),
      rawICAO = "BA0001",
      rawIATA = "BAA0001",
      origin = PortCode("JFK"),
      pcpTime = Option(1451655000000L), // 2016-01-01 13:30:00 UTC
      scheduled = SDate("2016-01-01T13:00").millisSinceEpoch,
      feedSources = Set(ApiFeedSource)
    )

    def withSplits(flights: Seq[Arrival]) = {
      flights.map(ApiFlightWithSplits(_, Set())).toList
    }

    "FlightsTables" - {
      def thead(timeline: Boolean = false): TagOf[TableSection] = <.thead(
        <.tr(
          if (timeline) <.th("Timeline") else TagMod(""),
          <.th("Flight"),
          <.th("Origin"),
          <.th("Country", ^.className := "country"),
          <.th("Gate / Stand", ^.className := "gate-stand"),
          <.th("Status", ^.className := "status"),
          <.th("Sch"),
          <.th("Est"),
          <.th("Act"),
          <.th("Est Chox"),
          <.th("Act Chox"),
          <.th("Est PCP"),
          <.th("Est PCP Pax"),
          <.th("e-Gates"),
          <.th("EEA"),
          <.th("Non-EEA"),
          <.th("Transfer Pax")
        ))

      val classesAttr = ^.className := "table table-responsive table-striped table-hover table-sm"
      val dataStickyAttr = VdomAttr("data-sticky") := "data-sticky"

      "Given a single flight then we see the FlightCode, " +
        "Origin, Gate/Stand, Status, Sch and other dates, and PCP Pax" - {
        val expected = <.div(
          <.div(),
          <.div(^.id := "toStick", ^.className := "container sticky",
            <.table(
              ^.id := "sticky",
              classesAttr,
              thead())),
          <.table(
            ^.id := "sticky-body",
            dataStickyAttr,
            classesAttr,
            thead(),
            <.tbody(
              <.tr(^.className := " before-now",
                <.td(^.className := "arrivals__table__flight-code", <.div(testFlight.flightCode)),
                <.td(testFlight.origin.toString),
                <.td(<.span(<.span())),
                <.td(s"${testFlight.gate.getOrElse("")} / ${testFlight.stand.getOrElse("")}"),
                <.td(testFlight.status.description),
                <.td(<.span(^.title := "2016-01-01 13:00", "13:00")), //sch
                <.td(<.span(^.title := "2016-01-01 13:05", "13:05")),
                <.td(<.span(^.title := "2016-01-01 13:10", "13:10")),
                <.td(<.span(^.title := "2016-01-01 13:15", "13:15")),
                <.td(<.span(^.title := "2016-01-01 13:20", "13:20")),
                <.td(<.div(<.span(^.title := "2016-01-01 13:30", "13:30"), " \u2192 ", <.span(^.title := "2016-01-01 13:37", "13:37"))),
                <.td(<.div(^.title := "Pax: 140 (150 - 10 transfer)\nMax: 200 ", ^.className := "right", 140)),
                <.td(<.span(0), ^.className := "queue-split pax-unknown egate-queue-pax right"),
                <.td(<.span(0), ^.className := "queue-split pax-unknown eeadesk-queue-pax right"),
                <.td(<.span(0), ^.className := "queue-split pax-unknown noneeadesk-queue-pax right"),
                <.td(<.div(testFlight.tranPax.get, ^.className := "right"))))))

        assertRenderedComponentsAreEqual(
          ArrivalsTable(timelineComponent = None)(FlightsWithSplitsTable.Props(withSplits(testFlight :: Nil), queuesWithoutFastTrack, hasEstChox = true, None, hasArrivalSourcesAccess = false, ViewLive, PcpPax.bestPaxEstimateWithApi, hasTransfer = true)),
          staticComponent(expected)())
      }

      "ArrivalsTableComponent has a hook for a timeline column" - {
        val timelineComponent: Arrival => VdomNode = (_: Arrival) => <.span("herebecallback")
        val expected =
          <.div(
            <.div(),
            <.div(^.id := "toStick", ^.className := "container sticky",
              <.table(
                ^.id := "sticky",
                classesAttr,
                thead(timeline = true))),
            <.table(
              ^.id := "sticky-body",
              dataStickyAttr,
              classesAttr,
              thead(timeline = true),
              <.tbody(
                <.tr(^.className := " before-now",
                  <.td(<.span("herebecallback")),
                  <.td(^.className := "arrivals__table__flight-code", <.div(testFlight.flightCode)),
                  <.td(testFlight.origin.toString),
                  <.td(<.span(<.span())),
                  <.td(s"${testFlight.gate.getOrElse("")} / ${testFlight.stand.getOrElse("")}"),
                  <.td(testFlight.status.description),
                  date(Option(testFlight.scheduled)),
                  date(testFlight.sstimated),
                  date(testFlight.actual),
                  date(testFlight.estimatedChox),
                  date(testFlight.actualChox),
                  <.td(<.div(<.span(^.title := "2016-01-01 13:30", "13:30"), " \u2192 ", <.span(^.title := "2016-01-01 13:37", "13:37"))),
                  <.td(<.div(^.title := "Pax: 140 (150 - 10 transfer)\nMax: 200 ", ^.className := "right", 140)),
                  <.td(<.span(0), ^.className := "queue-split pax-unknown egate-queue-pax right"),
                  <.td(<.span(0), ^.className := "queue-split pax-unknown eeadesk-queue-pax right"),
                  <.td(<.span(0), ^.className := "queue-split pax-unknown noneeadesk-queue-pax right"),
                  <.td(<.div(testFlight.tranPax.get, ^.className := "right"))))))

        assertRenderedComponentsAreEqual(
          ArrivalsTable(Option(timelineComponent))(FlightsWithSplitsTable.Props(withSplits(testFlight :: Nil), queuesWithoutFastTrack, hasEstChox = true, None, hasArrivalSourcesAccess = false, ViewLive, PcpPax.bestPaxEstimateWithApi, hasTransfer = true)),
          staticComponent(expected)())
      }

      "ArrivalsTableComponent has a hook for an origin portCode mapper" - {
        "Simple hook " - {
          val expected = <.div(
            <.div(),
            <.div(^.id := "toStick", ^.className := "container sticky",
              <.table(
                ^.id := "sticky",
                classesAttr,
                thead())),
            <.table(
              ^.id := "sticky-body",
              dataStickyAttr,
              classesAttr,
              thead(),
              <.tbody(
                <.tr(^.className := " before-now",
                  <.td(^.className := "arrivals__table__flight-code", <.div(testFlight.flightCode)),
                  <.td(<.span(^.title := "JFK, New York, USA", testFlight.origin.toString)), <.td(<.span(<.span())),
                  <.td(s"${testFlight.gate.getOrElse("")} / ${testFlight.stand.getOrElse("")}"),
                  <.td(testFlight.status.description),
                  date(Option(testFlight.scheduled)),
                  date(testFlight.sstimated),
                  date(testFlight.actual),
                  date(testFlight.estimatedChox),
                  date(testFlight.actualChox),
                  <.td(<.div(<.span(^.title := "2016-01-01 13:30", "13:30"), " \u2192 ", <.span(^.title := "2016-01-01 13:37", "13:37"))),
                  <.td(<.div(^.title := "Pax: 140 (150 - 10 transfer)\nMax: 200 ", ^.className := "right", 140)),
                  <.td(<.span(0), ^.className := "queue-split pax-unknown egate-queue-pax right"),
                  <.td(<.span(0), ^.className := "queue-split pax-unknown eeadesk-queue-pax right"),
                  <.td(<.span(0), ^.className := "queue-split pax-unknown noneeadesk-queue-pax right"),
                  <.td(<.div(testFlight.tranPax.get, ^.className := "right"))))))

          def originMapperComponent(portCode: PortCode): VdomNode = <.span(^.title := "JFK, New York, USA", portCode.toString)

          val table = ArrivalsTable(timelineComponent = None,
            originMapper = port => originMapperComponent(port)
          )(FlightsWithSplitsTable.Props(withSplits(testFlight :: Nil), queuesWithoutFastTrack, hasEstChox = true, None, hasArrivalSourcesAccess = false, ViewLive, PcpPax.bestPaxEstimateWithApi, hasTransfer = true))

          assertRenderedComponentsAreEqual(table, staticComponent(expected)())
        }
        "Unit tests for airportOrigin Hook" - {
          val airportInfos = Map[String, Pot[AirportInfo]](
            "JFK" -> Ready(AirportInfo("Johnny Frank Kelvin", "Bulawayo", "Zimbabwe", "JFK")))
          val originTooltip = FlightTableComponents.airportCodeTooltipText(airportInfos) _

          'TooltipFound - {
            val actual = originTooltip("JFK")
            val expected = "Johnny Frank Kelvin, Bulawayo, Zimbabwe"
            assert(actual == expected)
          }
          'TooltipNotFond - {
            val actual = originTooltip("NFD")
            val expected = "waiting for info..."
            assert(actual == expected)
          }
        }
        "Component test for airportMapper" - {
          val airportInfos = Map[String, Pot[AirportInfo]](
            "JFK" -> Ready(AirportInfo("Johnny Frank Kelvin", "Bulawayo", "Zimbabwe", "JFK")))
          val expected: VdomElement = <.span(^.title := "Johnny Frank Kelvin, Bulawayo, Zimbabwe", "JFK")
          val actual = FlightTableComponents.airportCodeComponent(airportInfos)("JFK")
          assertRenderedComponentsAreEqual(staticComponent(actual)(), staticComponent(expected)())
        }
      }
    }


    "FlightsLTNTables" - {
      def thead(timeline: Boolean = false): TagOf[TableSection] = <.thead(
        <.tr(
          if (timeline) <.th("Timeline") else TagMod(""),
          <.th("Flight"),
          <.th("Origin"),
          <.th("Country", ^.className := "country"),
          <.th("Gate / Stand", ^.className := "gate-stand"),
          <.th("Status", ^.className := "status"),
          <.th("Sch"),
          <.th("Est"),
          <.th("Act"),
          <.th("Est Chox"),
          <.th("Act Chox"),
          <.th("Est PCP"),
          <.th("Est PCP Pax"),
          <.th("e-Gates"),
          <.th("EEA"),
          <.th("Non-EEA")
        ))

      val classesAttr = ^.className := "table table-responsive table-striped table-hover table-sm"
      val dataStickyAttr = VdomAttr("data-sticky") := "data-sticky"

      "Given a LTN flight then we see the FlightCode, " +
        "Origin, Gate/Stand, Status, Sch and other dates, and PCP Pax and no Transfer Pax column" - {
        val expected = <.div(
          <.div(),
          <.div(^.id := "toStick", ^.className := "container sticky",
            <.table(
              ^.id := "sticky",
              classesAttr,
              thead())),
          <.table(
            ^.id := "sticky-body",
            dataStickyAttr,
            classesAttr,
            thead(),
            <.tbody(
              <.tr(^.className := " before-now",
                <.td(^.className := "arrivals__table__flight-code", <.div(testLTNFlight.flightCode)),
                <.td(testLTNFlight.origin.toString),
                <.td(<.span(<.span())),
                <.td(s"${testLTNFlight.gate.getOrElse("")} / ${testLTNFlight.stand.getOrElse("")}"),
                <.td(testLTNFlight.status.description),
                <.td(<.span(^.title := "2016-01-01 13:00", "13:00")), //sch
                <.td(<.span(^.title := "2016-01-01 13:05", "13:05")),
                <.td(<.span(^.title := "2016-01-01 13:10", "13:10")),
                <.td(<.span(^.title := "2016-01-01 13:15", "13:15")),
                <.td(<.span(^.title := "2016-01-01 13:20", "13:20")),
                <.td(<.div(<.span(^.title := "2016-01-01 13:30", "13:30"), " \u2192 ", <.span(^.title := "2016-01-01 13:37", "13:37"))),
                <.td(<.div(^.title := "Pax: 140 (150 - 10 transfer)\nMax: 200 ", ^.className := "right", 140)),
                <.td(<.span(0), ^.className := "queue-split pax-unknown egate-queue-pax right"),
                <.td(<.span(0), ^.className := "queue-split pax-unknown eeadesk-queue-pax right"),
                <.td(<.span(0), ^.className := "queue-split pax-unknown noneeadesk-queue-pax right")))))

        assertRenderedComponentsAreEqual(
          ArrivalsTable(timelineComponent = None)(FlightsWithSplitsTable.Props(withSplits(testFlight :: Nil), queuesWithoutFastTrack, hasEstChox = true, None, hasArrivalSourcesAccess = false, ViewLive, PcpPax.bestPaxEstimateWithApi, hasTransfer = false)),
          staticComponent(expected)())
      }

    }

  }

  def assertRenderedComponentsAreEqual[P](rc: Unmounted[P, Unit, Unit], expected: Unmounted[Unit, Unit, Unit]): Unit = {
    ReactTestUtils.withRenderedIntoDocument(rc) {
      real =>
        ReactTestUtils.withRenderedIntoDocument(expected) {
          simple =>
            val actualHtml = real.outerHtmlScrubbed()
            val simpleHtml = simple.outerHtmlScrubbed()
            assert(actualHtml == simpleHtml)
        }
    }
  }

  def staticComponent(staticVdomElement: => html_<^.VdomElement): Component[Unit, Unit, Unit, CtorType.Nullary] = {
    ScalaComponent.builder[Unit]("Expected")
      .renderStatic(
        staticVdomElement
      ).build
  }
}
