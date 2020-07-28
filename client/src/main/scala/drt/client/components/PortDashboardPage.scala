package drt.client.components

import diode.data.Pot
import diode.react.ModelProxy
import drt.client.SPAMain.{Loc, PortDashboardLoc}
import drt.client.modules.GoogleEventTracker
import drt.client.services.JSDateConversions.SDate
import drt.client.services.SPACircuit
import drt.shared._
import drt.shared.api.Arrival
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CtorType, ReactEventFromInput, ScalaComponent}
import d3v4._

import scala.scalajs.js._


object PortDashboardPage {

  case class Props(router: RouterCtl[Loc], dashboardPage: PortDashboardLoc)

  case class DisplayPeriod(start: SDateLike, end: SDateLike)

  object DisplayPeriod {
    def apply(start: SDateLike, hours: Int = 3): DisplayPeriod = DisplayPeriod(start, start.addHours(hours))
  }

  case class PortDashboardModel(
                                 airportConfig: Pot[AirportConfig],
                                 portState: Pot[PortState],
                                 featureFlags: Pot[Map[String, Boolean]]
                               )

  val component: Component[Props, Unit, Unit, CtorType.Props] = ScalaComponent.builder[Props]("PortDashboard")
    .render_P(p => {

      val modelRCP = SPACircuit.connect(rm => PortDashboardModel(rm.airportConfig, rm.portStatePot, rm.featureFlags))

      modelRCP { modelMP: ModelProxy[PortDashboardModel] =>
        val portDashboardModel: PortDashboardModel = modelMP()
        <.div(^.className := "terminal-summary-dashboard",

          portDashboardModel.airportConfig.renderReady(portConfig => {

            val (queues, paxTypeAndQueueOrder, terminals) = (portConfig.queuesByTerminal, portConfig.terminalPaxSplits, portConfig.terminals)

            val currentPeriodStart = DashboardTerminalSummary.windowStart(SDate.now())
            val periods = List(
              DisplayPeriod(currentPeriodStart),
              DisplayPeriod(currentPeriodStart.addHours(3)),
              DisplayPeriod(currentPeriodStart.addHours(6))
            )

            def displayPeriod = periods(p.dashboardPage.period.getOrElse(0))

            def switchDashboardPeriod(period: Int) = (_: ReactEventFromInput) => {
              GoogleEventTracker.sendEvent("dashboard", "Switch Period", period.toString)
              p.router.set(p.dashboardPage.copy(period = Option(period)))
            }

            <.div(
              <.div(
                <.div(^.className := "btn-group no-gutters", VdomAttr("data-toggle") := "buttons",
                  periods.zipWithIndex.map {
                    case (p, index) => <.div(
                      ^.className := s"btn btn-primary${if (p == displayPeriod) " active" else ""}",
                      s"${p.start.prettyTime()}-${p.end.prettyTime()}", ^.onClick ==> switchDashboardPeriod(index)
                    )
                  }.toTagMod)),
              <.div(^.id := "graphic"),
              terminals.map { terminalName =>
                <.div(
                  <.h3(s"Terminal $terminalName"),
                  portDashboardModel.portState.render(portState => {
                    portDashboardModel.featureFlags.render(_ => {
                      val portStateForDashboard = portState.windowWithTerminalFilter(
                        displayPeriod.start,
                        displayPeriod.end,
                        portConfig.queuesByTerminal.filterKeys(_ == terminalName)
                      )
                      val scheduledFlightsInTerminal = portStateForDashboard
                        .flights
                        .values
                        .filterNot(_.apiFlight.isCancelled)
                        .toList
                      val terminalCrunchMinutes = portStateForDashboard.crunchMinutes.values.toList
                      val terminalStaffMinutes = portStateForDashboard.staffMinutes.values.toList
                      val terminalQueuesInOrder = Queues.inOrder(queues.getOrElse(terminalName, Seq()))

                      portDashboardModel.featureFlags.renderReady(ff => {

                        val pcpPaxFn: Arrival => Int = PcpPax.bestPaxEstimateWithApi

                        DashboardTerminalSummary(
                          DashboardTerminalSummary.Props(scheduledFlightsInTerminal,
                            terminalCrunchMinutes,
                            terminalStaffMinutes,
                            terminalName,
                            paxTypeAndQueueOrder(terminalName).splits.map(_.paxType),
                            terminalQueuesInOrder,
                            displayPeriod.start,
                            displayPeriod.end,
                            pcpPaxFn
                          )
                        )
                      }
                      )
                    })
                  })
                )
              }.toTagMod
            )

          }))
      }
    })
    .componentWillReceiveProps(p => Callback {
      GoogleEventTracker.sendPageView(s"dashboard${p.nextProps.dashboardPage.period.map(period => s"/$period").getOrElse("")}")
    })
    .componentDidMount(p => Callback {
      GoogleEventTracker.sendPageView(s"dashboard${p.props.dashboardPage.period.map(period => s"/$period").getOrElse("")}")
    })
    .componentDidMount(_ => Callback {

      val data = Map[String, Int](
        "Apples" -> 20,
        "Bananas" -> 12,
        "Grapes" -> 19,
        "Lemons" -> 5,
        "Limes" -> 16,
        "Oranges" -> 26,
        "Pears" -> 30)

      object margin {
        val top = 15
        val right = 25
        val bottom = 15
        val left = 60
      }
      val width = 960 - margin.left - margin.right
      val height = 500 - margin.top - margin.bottom

      val svg = d3.select("#graphic").append("svg").attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

      var x: d3scale.LinearScale = d3.scaleLinear().range(Array(0d, width.toDouble)).domain(Array(0, 30))
      var y: d3scale.OrdinalScale = d3.scaleOrdinal(Array.apply(data.keys.toSeq:_*)).domain(Array(data.keys.toSeq:_*))//.rangeRoundBands(Array(height, 0), .1).domain(data.keys);
//      var yAxis = d3.svg.axis()
//        .scale(y)
//        no tick marks
//        .tickSize(0)
//        .orient("left");
      var bars = svg.selectAll(".bar")
        .data(Array.apply(data.values.toSeq:_*))
        .enter()
        .append("g")

      bars.append("rect")
        .attr("class", "bar")
//        .attr("y", (d: data) {
//          return y(d.name);
//        })
        .attr("height", y.rangeBand())
        .attr("x", 0)
        .attr("width", function (d) {
          return x(d.value);
        });
    })
    .build

  def apply(router: RouterCtl[Loc], dashboardPage: PortDashboardLoc = PortDashboardLoc(None)): VdomElement = component(Props(router, dashboardPage))
}
