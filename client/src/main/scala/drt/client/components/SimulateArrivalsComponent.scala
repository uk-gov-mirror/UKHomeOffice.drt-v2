package drt.client.components

import drt.client.SPAMain
import drt.client.actions.Actions.GetSimulation
import drt.client.modules.GoogleEventTracker
import drt.client.services.JSDateConversions.SDate
import drt.client.services.SPACircuit
import drt.shared.CrunchApi.DeskRecMinutes
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.dates.LocalDate
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.util.{Success, Try}

object SimulateArrivalsComponent {

  case class Props(date: LocalDate, terminal: Terminal, airportConfig: AirportConfig)


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

          val modelRCP = SPACircuit.connect(_.simulationResult)

          modelRCP { modelMP =>
            val simulationPot = modelMP()

            <.div(^.id := "simulation",
              simulationPot.render(s => {
//                val byQ = inQueuesBy15Minutes(s.deskRecMinutes, props.terminal)
//                println(s"byQ: $byQ")
//                byQ.map {
//                  case (q, cms) =>
//
//                    <.div(
//                      <.h3(Queues.queueDisplayNames(q)),
//                      //                      ChartJSComponent.Bar(
//                      //                      ChartJsProps(
//                      //                        data = ChartJsData(
//                      //                          cms.map(m => SDate(m.minute).toHoursAndMinutes),
//                      //                          cms.map(_.paxLoad),
//                      //                          "Pax"
//                      //                        ),
//                      //                        300,
//                      //                        300
//                      //                      )
//                      //                    )
//                    )
//                }

                <.div("nothing")
              }),
            )
          }
        })
      )
    }
    .componentDidMount(_ => Callback {
      GoogleEventTracker.sendPageView(s"Arrival Simulations Page")
    }).build

  def apply(date: LocalDate, terminal: Terminal, airportConfg: AirportConfig): VdomElement = component(Props(date, terminal, airportConfg))

  def inQueuesBy15Minutes(mins: DeskRecMinutes, terminal: Terminal): Map[Queues.Queue, List[CrunchApi.CrunchMinute]] = {
    val ps = PortState(List(), mins.minutes.map(_.toMinute), List())
    val start = mins.minutes.map(_.minute).min
    val queues = mins.minutes.map(_.queue).toList

    ps
      .crunchSummary(SDate(start), MilliTimes.fifteenMinuteSlotsInDay, MilliTimes.fifteenMinutesMillis, terminal, queues)
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
