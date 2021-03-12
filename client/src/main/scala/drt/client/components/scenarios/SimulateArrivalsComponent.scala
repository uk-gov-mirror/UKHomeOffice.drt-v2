package drt.client.components.scenarios

import drt.client.SPAMain
import drt.client.actions.Actions.GetSimulation
import drt.client.components.ChartJSComponent._
import drt.client.components.{ChartJSComponent, potReactForwarder}
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

  case class State(
                    simulationParams: SimulationParams,
                    displayDialogue: Boolean
                  ) {
    def handleClickOpen = copy(displayDialogue = true)

    def handleClickClose = copy(displayDialogue = false)
  }

  case class Props(
                    date: LocalDate,
                    terminal: Terminal,
                    airportConfig: AirportConfig,
                    portState: PortState,
                  )


  implicit val propsReuse: Reusability[Props] = Reusability.by((props: Props) => {
    props.portState.hashCode()
  })

  implicit val stateReuse: Reusability[State] = Reusability.by_==[State]
  implicit val paramsReuse: Reusability[SimulationParams] = Reusability.by_==[SimulationParams]
  implicit val dateReuse: Reusability[LocalDate] = Reusability.derive[LocalDate]


  val component = ScalaComponent.builder[Props]("SimulationConfiguration")
    .initialStateFromProps(p =>
      State(SimulationParams(p.terminal, p.date, p.airportConfig), true)
    )
    .renderPS { (scope, props, state) =>

      <.div(
        <.div(<.h2("Arrival Simulations")),
        <.div(^.className := "row",

          <.div(
            ^.className := "form-group row  col-sm-10",
            <.label(^.className := "col-sm-3", ^.htmlFor := "passenger-weighting", "Passenger weighting"),
            <.input(^.tpe := "number",
              ^.step := "0.01",
              ^.id := "passenger-weighting",
              ^.defaultValue := 1.0,
              ^.onChange ==> ((e: ReactEventFromInput) => Try(e.target.value.toDouble) match {
                case Success(weight) =>
                  scope.setState(state.copy(simulationParams = state.simulationParams.copy(passengerWeighting = weight)))
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
              ^.defaultValue := state.simulationParams.eGateBanksSize,
              ^.onChange ==> ((e: ReactEventFromInput) => Try(e.target.value.toInt) match {
                case Success(bankSize) =>
                  scope.setState(state.copy(simulationParams = state.simulationParams.copy(passengerWeighting = bankSize)))
                case _ =>
                  Callback.empty
              })
            )
          ),
          <.div(
            ^.className := "form-group row col-sm-10",
            <.legend(^.className := "pt-0", "Processing times (seconds)"),
            <.div(^.className := "",
              state.simulationParams.processingTimes.map {
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
                            scope.setState(state.copy(simulationParams = state.simulationParams.copy(processingTimes = state.simulationParams.processingTimes + (ptq -> procTimes))))
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
              state.simulationParams.slaByQueue.map {
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
                            scope.setState(state.copy(simulationParams = state.simulationParams.copy(slaByQueue = state.simulationParams.slaByQueue + (q -> sla))))
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
              state.simulationParams.minDesks.keys.map {
                case q =>
                  <.div(
                    ^.className := "form-check",
                    <.label(
                      ^.className := "col-sm-3",
                      s"${Queues.queueDisplayNames(q)}"
                    ),
                    <.input(^.tpe := "number",
                      ^.id := s"${q}_min",
                      ^.defaultValue := state.simulationParams.minDesks(q),
                      ^.onChange ==> ((e: ReactEventFromInput) => Try(e.target.value.toInt) match {
                        case Success(min) =>
                          scope.setState(state.copy(simulationParams = state.simulationParams.copy(minDesks = state.simulationParams.minDesks + (q -> min))))
                        case _ => Callback.empty
                      })
                    ),
                    <.input(^.tpe := "number",
                      ^.id := s"${q}_max",
                      ^.defaultValue := state.simulationParams.maxDesks(q),
                      ^.onChange ==> ((e: ReactEventFromInput) => Try(e.target.value.toInt) match {
                        case Success(max) =>
                          scope.setState(state.copy(simulationParams = state.simulationParams.copy(maxDesks = state.simulationParams.maxDesks + (q -> max))))
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
              ^.onClick --> Callback(SPACircuit.dispatch(GetSimulation(state.simulationParams)))
            )
          ),
          <.div(^.className := "form-group row col-sm-10",
            <.a(^.className := "btn btn-primary",
              ^.id := "export-simulation",
              ^.target := "_blank",
              ^.href := SPAMain.absoluteUrl(s"export/desk-rec-simulation?${state.simulationParams.toQueryStringParams}"),
              "Export"
            )
          )
        ),
        <.div(^.className := "row",
          <.div(^.className := "col",
            SimulationChartComponent(state.simulationParams, props.portState, props.terminal)
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_ => Callback {
      GoogleEventTracker.sendPageView(s"Arrival Simulations Page")
    }).build

  def apply(date: LocalDate, terminal: Terminal, airportConfg: AirportConfig, portState: PortState): VdomElement =
    component(Props(date, terminal, airportConfg, portState))
}


