package drt.client.components.scenarios

import drt.client.components.ChartJSComponent._
import drt.client.components.Helpers.StringExtended
import drt.client.components.{ChartJSComponent, potReactForwarder}
import drt.client.services.JSDateConversions.SDate
import drt.client.services.SPACircuit
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared._
import japgolly.scalajs.react.component.Js.{RawMounted, UnmountedWithRawType}
import japgolly.scalajs.react.vdom.all.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ReactEvent, ScalaComponent}
import scalacss.ScalaCssReactImplicits

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichGenTraversableOnce

//object CustomizedTabs extends ScalaCssReactImplicits {
//
//
//  case class Props()
//
//  //  case class State(value: js.Any = 0) {
//  //    def handleChange(value: js.Any) = copy(value = value)
//  //  }
//  //
//  class Backend() {
//    //    def handleChange: (ReactEvent, js.Any) => Callback = (_, value) => {
//    //      t.modState(_.handleChange(value))
//    //    }
//
//
//    def render(props: Props): VdomElement = {
//
//
//      div(
//        div(
//          div(
//            //            WithPropsAndTagsMods.toVdomNode(MuiTabs(onChange = handleChange)(
//            //              ^.value := "",
//            //              MuiTab(disableRipple = true, label = "Tab 1".toVdom),
//            //              MuiTab(disableRipple = true, label = "Tab 2".toVdom),
//            //              MuiTab(disableRipple = true, label = "Tab 3".toVdom)
//            //            )),
//            <.div("Ant Design UI powered by Material-UI".toVdom)
//          )
//        )
//      )
//    }
//
//    private val component = ScalaComponent.builder[Props]("CustomizedTabs")
//      //    .initialState(State())
//      .renderBackend[Backend]
//      .build
//  }
//  def apply() = component(Props())
//
//}

object SimulationChartComponent extends ScalaCssReactImplicits {

  case class Props(
                    simulationParams: SimulationParams,
                    portState: PortState,
                    terminal: Terminal
                  )

  case class State(activeTab: js.Any = 0) {
    def handleChange(tab: js.Any) = copy(activeTab = tab)

    val numberValue = activeTab.asInstanceOf[Int]

    def isSelected(index: Int) = index == numberValue
  }

  val component = ScalaComponent.builder[Props]("SimulationChartComponent")
    .initialState(State())
    .renderPS { (scope, props, state) =>


      val modelRCP = SPACircuit.connect(m => m.simulationResult)

      def handleChange: (ReactEvent, js.Any) => Callback = (_, value) => {
        scope.modState(_.handleChange(value))
      }

      def chartList(qCharts: List[(Queue, UnmountedWithRawType[ChartJSComponent.Props, Null, RawMounted[ChartJSComponent.Props, Null]])]) = {
        qCharts.zipWithIndex.map {
          case ((_, c), i) => c.when(state.isSelected(i))
        }
      }

      modelRCP { modelMP =>
        val simulationPot = modelMP()

        <.div(
          simulationPot.render(simulationResult => {

            val qCharts = resultToQueueCharts(props, simulationResult).toList
            <.div(
              qCharts.map {
                case (q, c) =>
                  <.div(
                    <.h3(Queues.queueDisplayNames(q).toVdom),
                    <.div(c)
                  )
              }.toVdomArray)


          })

        )


      }
    }
    .build

  def resultToQueueCharts(props: Props, simulationResult: SimulationResult) = {
    val startDate = SDate(simulationResult.params.date)
    val portStateQueueCrunchMinutes = inQueuesBy15Minutes(
      props.portState.window(startDate, startDate.getLocalNextMidnight),
      startDate,
      simulationResult.queueToCrunchMinutes.keySet.toList,
      props.terminal
    )
    simulationResult.queueToCrunchMinutes.map {
      case (q, simulationCrunchMinutes) =>
        val labels = simulationCrunchMinutes.map(m => SDate(m.minute).toHoursAndMinutes)

        val pscmForQ = portStateQueueCrunchMinutes(q)
        val dataSets: Seq[ChartJsDataSet] = List(
          ChartJsDataSet.bar(
            "Simulation Pax",
            simulationCrunchMinutes.map(m => Math.round(m.paxLoad).toDouble),
            RGBA.blue1
          ),
          ChartJsDataSet.line(
            "Simulation Workload Minutes",
            simulationCrunchMinutes.map(m => Math.round(m.paxLoad).toDouble),
            RGBA.blue2
          ),
          ChartJsDataSet.line(
            "Simulation Wait Times",
            simulationCrunchMinutes.map(m => Math.round(m.waitTime).toDouble),
            RGBA.blue3
          ),
          ChartJsDataSet.bar(
            "Predicted Pax",
            pscmForQ.map(m => Math.round(m.paxLoad).toDouble),
            RGBA.red1
          ),
          ChartJsDataSet.line(
            "Predicted Workload Minutes",
            pscmForQ.map(m => Math.round(m.paxLoad).toDouble),
            RGBA.red2
          ),
          ChartJsDataSet.line(
            "Predicted Wait Times",
            pscmForQ.map(m => Math.round(m.waitTime).toDouble),
            RGBA.red3
          ),
        )


        q -> ChartJSComponent.Bar(
          ChartJsProps(
            data = ChartJsData(dataSets, Option(labels)),
            300,
            150,
            ChartJsOptions.withMultipleDataSets("Simulation")
          )
        )


    }.toMap
  }

  def minutesToQueueDataSets(cms: List[CrunchApi.CrunchMinute]) = {
    val paxPerSlot = cms.map(m => Math.round(m.paxLoad).toDouble)
    val paxDataSet = ChartJsDataSet(
      data = paxPerSlot.toJSArray,
      label = "Pax arriving at PCP",
      backgroundColor = "rgba(102,102,255,0.2)",
      borderColor = "rgba(102,102,255,1)",
      borderWidth = 1,
      hoverBackgroundColor = "rgba(102,102,255,0.4)",
      hoverBorderColor = "rgba(102,102,255,1)",
    )

    val workPerSlot = cms.map(m => Math.round(m.workLoad).toDouble)
    val workDataSet = ChartJsDataSet(
      data = workPerSlot.toJSArray,
      label = "Workload",
      backgroundColor = "rgba(160,160,160,0.2)",
      borderColor = "rgba(160,160,160,1)",
      borderWidth = 1,
      hoverBackgroundColor = "rgba(160,160,160,0.4)",
      hoverBorderColor = "rgba(160,160,160,1)",
      `type` = "line"
    )

    val waitTime = cms.map(m => Math.round(m.waitTime).toDouble)
    val waitDataSet = ChartJsDataSet(
      data = waitTime.toJSArray,
      label = "Wait Times",
      backgroundColor = "rgba(255,51,51,0.2)",
      borderColor = "rgba(255,51,51,1)",
      borderWidth = 1,
      hoverBackgroundColor = "rgba(255,51,51,0.4)",
      hoverBorderColor = "rgba(255,51,51,1)",
      `type` = "line"
    )

    Seq(paxDataSet, workDataSet, waitDataSet)
  }


  def inQueuesBy15Minutes(ps: PortState, start: SDateLike, queues: List[Queue], terminal: Terminal): Map[Queues.Queue, List[CrunchApi.CrunchMinute]] = {
    println(s"summarising ${ps.crunchMinutes.size} minutes")
    ps
      .crunchSummary(start, MilliTimes.fifteenMinuteSlotsInDay, 15, terminal, queues)
      .values
      .flatten
      .toList
      .collect {
        case (_, cm) => cm
      }
      .groupBy(_.queue)
      .mapValues(_.sortBy(_.minute))
  }

  def apply(
             simulationParams: SimulationParams,
             portState: PortState,
             terminal: Terminal
           ): VdomElement = component(Props(simulationParams, portState, terminal))


}
