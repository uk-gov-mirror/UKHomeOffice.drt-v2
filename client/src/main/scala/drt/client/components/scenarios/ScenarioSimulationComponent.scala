package drt.client.components.scenarios

import drt.client.SPAMain
import drt.client.actions.Actions.{GetSimulation, ReSetSimulation}
import drt.client.components.Helpers._
import drt.client.components.styles.DefaultFormFieldsStyle
import drt.client.modules.GoogleEventTracker
import drt.client.services.SPACircuit
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.dates.LocalDate
import io.kinoplan.scalajs.react.material.ui.core.{MuiLink, _}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.Attr
import japgolly.scalajs.react.vdom.all.{EmptyVdom, `type`, disabled, id, onChange, onClick, value}
import japgolly.scalajs.react.vdom.html_<^.{<, _}
import scalacss.ScalaCssReactImplicits

import scala.scalajs.js.JSConverters.JSRichOption
import scala.util.{Success, Try}

object ScenarioSimulationComponent extends ScalaCssReactImplicits {

  val v: TagMod = value := 1

  val steps = List("Passenger numbers", "Processing Times", "Queue SLAs", "Configure Desk Availability")


  case class State(
                    simulationParams: SimulationParams,
                    openConfig: Boolean,
                    activeStep: Int = 0,
                    skipped: Set[Int] = Set.empty[Int]
                  ) {

    val isFinish = activeStep == steps.length

    val notFinish = !isFinish

    val isFinal = activeStep == steps.length - 1

    val nextTitle = if (isFinal) "Finish" else "Next"

    val isBackDisabled = activeStep <= 0

    def isStepSkipped(step: Int = activeStep) = skipped.contains(step)

    def isStepOptional(step: Int = activeStep) = false

    def isStepActive(step: Int) = step == activeStep

    def handleNext = {
      if (isFinal) {
        SPACircuit.dispatch(GetSimulation(simulationParams))
        copy(
          activeStep = activeStep + 1
        )
      } else {
        copy(
          activeStep = activeStep + 1
        )
      }
    }

    def handleStep(step: Int) = copy(
      activeStep = step
    )

    def handleBack = copy(activeStep = activeStep - 1)

    def handleSkip = copy(
      skipped = skipped + activeStep,
      activeStep = activeStep + 1
    )

    def handleReset = {
      SPACircuit.dispatch(ReSetSimulation)
      copy(activeStep = 0)
    }
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
      State(SimulationParams(p.terminal, p.date, p.airportConfig), false)
    )
    .renderPS {

      (scope, props, state) =>

        def changePassengerWeighting(e: ReactEventFromInput): Callback = Try(e.target.value.toDouble) match {
          case Success(weight) =>
            scope.setState(state.copy(simulationParams = state.simulationParams.copy(passengerWeighting = weight)))
          case _ =>
            Callback.empty
        }

        def changeBankSize(e: ReactEventFromInput): Callback = Try(e.target.value.toInt) match {
          case Success(bs) =>
            scope.setState(state.copy(simulationParams = state.simulationParams.copy(eGateBanksSize = bs)))
          case _ =>
            Callback.empty
        }

        def changeProcessingTimes(ptq: PaxTypeAndQueue): ReactEventFromInput => Callback = (e: ReactEventFromInput) =>
          Try(e.target.value.toInt) match {
            case Success(procTimes) =>
              scope.setState(state.copy(
                simulationParams = state
                  .simulationParams
                  .copy(processingTimes = state.simulationParams.processingTimes + (ptq -> procTimes))
              ))
            case _ => Callback.empty
          }

        def changeQueueSla(q: Queues.Queue): ReactEventFromInput => Callback = (e: ReactEventFromInput) =>
          Try(e.target.value.toInt) match {
            case Success(sla) if sla >= 3 =>
              scope.setState(state.copy(
                simulationParams = state
                  .simulationParams
                  .copy(slaByQueue = state.simulationParams.slaByQueue + (q -> sla))
              ))
            case _ => Callback.empty
          }

        def changeMinDesks(q: Queues.Queue): ReactEventFromInput => Callback = {
          (e: ReactEventFromInput) =>
            Try(e.target.value.toInt) match {
              case Success(min) =>
                scope.setState(state.copy(
                  simulationParams = state
                    .simulationParams
                    .copy(minDesks = state.simulationParams.minDesks + (q -> min))
                ))
              case _ => Callback.empty
            }
        }

        def changeMaxDesks(q: Queues.Queue): ReactEventFromInput => Callback = {
          (e: ReactEventFromInput) =>
            Try(e.target.value.toInt) match {
              case Success(max) =>
                scope.setState(state.copy(
                  simulationParams = state
                    .simulationParams
                    .copy(maxDesks = state.simulationParams.maxDesks + (q -> max))
                ))
              case _ => Callback.empty
            }
        }

        def step1 = {

          <.div(
            MuiTextField(
              label = "Passenger weighting".toVdom,
              margin = MuiTextField.Margin.normal
            )(
              DefaultFormFieldsStyle.textField,
              `type` := "number",
              id := "passenger-weighting",
              v,
              onChange ==> changePassengerWeighting
            ))
        }

        def step2 = {
          <.div(^.className := "",
            state.simulationParams.processingTimes.map {
              case (ptq, _) =>
                <.div(^.className := "form-check",
                  MuiTextField(
                    label = s"${PaxTypes.displayName(ptq.passengerType)} to ${Queues.queueDisplayNames(ptq.queueType)}".toVdom,
                    margin = MuiTextField.Margin.normal
                  )(
                    DefaultFormFieldsStyle.textField,
                    `type` := "number",
                    id := "egate-bank-size",
                    value := state.simulationParams.processingTimes(ptq),
                    onChange ==> changeProcessingTimes(ptq)
                  )
                )
            }.toTagMod
          )
        }

        def step3 = {
          <.div(
            state.simulationParams.slaByQueue.map {
              case (q, _) =>
                <.div(^.className := "form-check",
                  MuiTextField(
                    label = s"${Queues.queueDisplayNames(q)} (at least 3 minutes)".toVdom,
                    margin = MuiTextField.Margin.normal
                  )(
                    DefaultFormFieldsStyle.textField,
                    `type` := "number",
                    id := "egate-bank-size",
                    value := state.simulationParams.slaByQueue(q),
                    onChange ==> changeQueueSla(q)
                  )
                )
            }.toTagMod
          )
        }

        def step4 = {
          <.div(
            state.simulationParams.minDesks.keys.map {
              case q =>
                <.div(
                  ^.className := "form-check",
                  MuiTextField(
                    label = s"${Queues.queueDisplayNames(q)} Min".toVdom,
                    margin = MuiTextField.Margin.normal
                  )(
                    DefaultFormFieldsStyle.textField,
                    `type` := "number",
                    id := s"${q}_min",
                    value := state.simulationParams.minDesks(q),
                    onChange ==> changeMinDesks(q)
                  ),
                  MuiTextField(
                    label = s"${Queues.queueDisplayNames(q)} Max".toVdom,
                    margin = MuiTextField.Margin.normal
                  )(
                    DefaultFormFieldsStyle.textField,
                    `type` := "number",
                    id := s"${q}_max",
                    value := state.simulationParams.maxDesks(q),
                    onChange ==> changeMaxDesks(q)
                  ),
                )
            }.toTagMod,
            <.div(
              MuiTextField(
                label = "E-Gate bank size".toVdom,
                margin = MuiTextField.Margin.normal
              )(
                DefaultFormFieldsStyle.textField,
                `type` := "number",
                id := "egate-bank-size",
                value := state.simulationParams.eGateBanksSize,
                onChange ==> changeBankSize
              )
            ),
          )
        }

        def getStepContent(step: Int) = step match {
          case 0 => step1
          case 1 => step2
          case 2 => step3
          case 3 => step4
          case _ => "Error".toVdom
        }

        def handleNext = scope.modState(_.handleNext)

        def handleBack = scope.modState(_.handleBack)

        def handleStep(step: Int) = scope.modState(_.handleStep(step))

        def handleSkip = scope.modState(_.handleSkip)

        def handleReset = scope.modState(_.handleReset)

        <.div(
          <.div(<.h2("Arrival Simulations")),
          MuiStepper(activeStep = state.activeStep, nonLinear = true)(
            steps.zipWithIndex.toVdomArray { case (label, index) =>
              val optional = if (state.isStepOptional(index)) {
                MuiTypography(variant = MuiTypography.Variant.caption)("Optional")
              } else EmptyVdom

              val completed = if (state.isStepSkipped(index)) Some(false) else None

              MuiStep(completed = completed.orUndefined)(Attr("key") := label,
                MuiStepLabel(optional = optional)(onClick --> handleStep(index), label)
              )
            }
          ),
          <.div(
            <.div(
              <.div(
                MuiPaper()(
                  DefaultFormFieldsStyle.simulationStepper, getStepContent(state.activeStep)),
                <.div(
                  MuiButton()(onClick --> handleBack, disabled := state.isBackDisabled, "Back"),
                  MuiButton(
                    variant = MuiButton.Variant.contained,
                    color = MuiButton.Color.primary,
                  )(onClick --> handleNext, state.nextTitle),
                  MuiButton(variant = MuiButton.Variant.raised)(onClick --> handleReset, "Reset"),
                  MuiLink()(
                    ^.target := "_blank",
                    ^.href := SPAMain.absoluteUrl(s"export/desk-rec-simulation?${state.simulationParams.toQueryStringParams}"),
                    "Simulation Export"
                  )
                )
              ),
            ),

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


