package drt.client.components

import drt.client.SPAMain
import drt.client.actions.Actions.GetSimulation
import drt.client.components.ChartJSComponent.{ChartJsData, ChartJsDataSet, ChartJsOptions, ChartJsProps}
import drt.client.modules.GoogleEventTracker
import drt.client.services.JSDateConversions.SDate
import drt.client.services.SPACircuit
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.dates.LocalDate
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js.JSConverters._
import scala.util.{Success, Try}

object SimulateArrivalsComponent {

  case class Props(
                    date: LocalDate,
                    terminal: Terminal,
                    airportConfig: AirportConfig,
                    portState: PortState
                  )


  implicit val propsReuse: Reusability[Props] = Reusability.by((props: Props) => {
    props.portState.hashCode()
  })

  implicit val stateReuse: Reusability[SimulationParams] = Reusability.by_==[SimulationParams]
  implicit val dateReuse: Reusability[LocalDate] = Reusability.derive[LocalDate]

  val component = ScalaComponent.builder[Props]("ArrivalSimulations")

    .initialStateFromProps(p => SimulationParams(p.terminal, p.date, p.airportConfig))
    .renderPS { (scope, props, state) =>

      <.div(<.div(<.h2("Arrival Simulations")),
        <.div(

          <.div(
            ^.className := "form-group row  col-sm-10",
            <.label(^.className := "col-sm-3", ^.htmlFor := "passenger-weighting", "Passenger weighting"),
            <.input(^.tpe := "number",
              ^.step := "0.01",
              ^.id := "passenger-weighting",
              ^.defaultValue := 1.0,
              ^.onChange ==> ((e: ReactEventFromInput) => Try(e.target.value.toDouble) match {
                case Success(weight) =>
                  scope.setState(state.copy(passengerWeighting = weight))
                case _ =>
                  Callback.empty
              })
            )
          ),
          <.div(
            ^.className := "form-group row  col-sm-10",
            <.label(^.className := "col-sm-3", ^.htmlFor := "egate-bank-size", "E-Gate bank size"),
            <.input(^.tpe := "number",
              ^.step := "1",
              ^.id := "egate-bank-size",
              ^.defaultValue := state.eGateBanksSize,
              ^.onChange ==> ((e: ReactEventFromInput) => Try(e.target.value.toInt) match {
                case Success(bankSize) =>
                  scope.setState(state.copy(passengerWeighting = bankSize))
                case _ =>
                  Callback.empty
              })
            )
          ),
          <.div(
            ^.className := "form-group row col-sm-10",
            <.legend(^.className := "pt-0", "Processing times (seconds)"),
            <.div(^.className := "",
              state.processingTimes.map {
                case (ptq, time) =>
                  <.div(^.className := "form-check",
                    <.label(
                      ^.className := "col-sm-3",
                      s"${PaxTypes.displayName(ptq.passengerType)} to ${Queues.queueDisplayNames(ptq.queueType)}"
                    ),
                    <.input(^.tpe := "number",
                      ^.defaultValue := time,
                      ^.id := ptq.key,
                      ^.onChange ==> ((e: ReactEventFromInput) =>
                        Try(e.target.value.toInt) match {
                          case Success(procTimes) =>
                            scope.setState(state.copy(processingTimes = state.processingTimes + (ptq -> procTimes)))
                          case _ => Callback.empty
                        }
                        )
                    )
                  )
              }.toTagMod
            )),
          <.div(
            ^.className := "form-group row col-sm-10",
            <.legend(^.className := "pt-0", "Queue SLAs (minutes)"),
            <.div(^.className := "",
              state.slaByQueue.map {
                case (q, sla) =>
                  <.div(^.className := "form-check",
                    <.label(
                      ^.className := "col-sm-3",
                      s"${Queues.queueDisplayNames(q)} (at least 3 minutes)"
                    ),
                    <.input(^.tpe := "number",
                      ^.defaultValue := sla,
                      ^.id := s"${q}_sla",
                      ^.min := "3",
                      ^.onChange ==> ((e: ReactEventFromInput) =>
                        Try(e.target.value.toInt) match {
                          case Success(sla) if sla >= 3 =>
                            scope.setState(state.copy(slaByQueue = state.slaByQueue + (q -> sla)))
                          case _ => Callback.empty
                        })
                    )
                  )
              }.toTagMod
            )),
          <.div(
            ^.className := "form-group row col-sm-10",
            <.legend(^.className := "pt-0", "Desks / Banks"),
            <.div(^.className := "",
              state.minDesks.keys.map {
                case q =>
                  <.div(
                    ^.className := "form-check",
                    <.label(
                      ^.className := "col-sm-3",
                      s"${Queues.queueDisplayNames(q)}"
                    ),
                    <.input(^.tpe := "number",
                      ^.id := s"${q}_min",
                      ^.defaultValue := state.minDesks(q),
                      ^.onChange ==> ((e: ReactEventFromInput) => Try(e.target.value.toInt) match {
                        case Success(min) =>
                          scope.setState(state.copy(minDesks = state.minDesks + (q -> min)))
                        case _ => Callback.empty
                      })
                    ),
                    <.input(^.tpe := "number",
                      ^.id := s"${q}_max",
                      ^.defaultValue := state.maxDesks(q),
                      ^.onChange ==> ((e: ReactEventFromInput) => Try(e.target.value.toInt) match {
                        case Success(max) =>
                          scope.setState(state.copy(maxDesks = state.maxDesks + (q -> max)))
                        case _ => Callback.empty
                      })
                    )
                  )
              }.toTagMod
            )
          ),
          <.div(^.className := "form-group row col-sm-10",
            <.a(^.className := "btn btn-primary",
              ^.id := "simulation",
              "Simulate",
              ^.onClick --> Callback(SPACircuit.dispatch(GetSimulation(state)))
            )
          ),
          <.div(^.className := "form-group row col-sm-10",
            <.a(^.className := "btn btn-primary",
              ^.id := "export-simulation",
              ^.target := "_blank",
              ^.href := SPAMain.absoluteUrl(s"export/desk-rec-simulation?${state.toQueryStringParams}"),
              "Export"
            )
          )
        ),
        <.div({

          val modelRCP = SPACircuit.connect(m => (
            m.simulationResult
            ))

          modelRCP { modelMP =>
            val simulationPot = modelMP()

            <.div(^.id := "simulation",
              simulationPot.render(simulationResult => {


                val startDate = SDate(simulationResult.params.date)
                val portStateQueueCrunchMinutes = inQueuesBy15Minutes(
                  props.portState.window(startDate, startDate.getLocalNextMidnight),
                  startDate,
                  simulationResult.queueToCrunchMinutes.keySet.toList,
                  props.terminal
                )
                simulationResult.queueToCrunchMinutes.map {
                  case (q, cms) =>
                    val labels = cms.map(m => SDate(m.minute).toHoursAndMinutes)

                    val simulationDataSets: Seq[ChartJsDataSet] = minutesToQueueDataSets(cms)
                    val portStateDataSets: Seq[ChartJsDataSet] = minutesToQueueDataSets(portStateQueueCrunchMinutes(q))


                    <.div(^.className := "simulation__chart-box",
                      <.h3(Queues.queueDisplayNames(q)),
                      ChartJSComponent.Bar(
                        ChartJsProps(
                          data = ChartJsData(List(simulationDataSets, portStateDataSets).flatten, Option(labels)),
                          300,
                          150,
                          ChartJsOptions.withMultipleDataSets("Simulation")
                        )
                      )
                    )
                }.toVdomArray
              }),
            )
          }
        })
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_ => Callback {
      GoogleEventTracker.sendPageView(s"Arrival Simulations Page")
    }).build

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

  def apply(date: LocalDate, terminal: Terminal, airportConfg: AirportConfig, portState: PortState): VdomElement =
    component(Props(date, terminal, airportConfg, portState))

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

}
