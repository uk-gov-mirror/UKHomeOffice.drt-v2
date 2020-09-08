package drt.client.components

import diode.UseValueEq
import diode.data.Pot
import drt.auth.LoggedInUser
import drt.client.SPAMain._
import drt.client.actions.Actions.RedirectToRequestAccess
import drt.client.services.SPACircuit
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CtorType, _}

object Layout {

  case class Props(ctl: RouterCtl[Loc], currentLoc: Resolution[Loc])

  case class LayoutModelItems(
                               userPot: Pot[LoggedInUser],
                               hasPortAccessPot: Pot[Boolean],
                               displayAlertDialog: Pot[Boolean]
                             ) extends UseValueEq

  val component: Component[Props, Unit, Unit, CtorType.Props] = ScalaComponent.builder[Props]("Layout")
    .renderP((_, props: Props) => {
      val layoutModelItemsRCP = SPACircuit.connect(m => LayoutModelItems(m.loggedInUserPot, m.userHasPortAccess, m.displayAlertDialog))
      layoutModelItemsRCP(layoutModelItemsMP => {
        val layoutModelItems: LayoutModelItems = layoutModelItemsMP()
        <.div(
          <.div(^.className := "topbar",
            <.div(^.className := "main-logo"),
            EnvironmentWarningComponent(),
            <.div(^.className := "alerts", AlertsComponent())
          ),
          <.div(
            layoutModelItems.userPot.renderReady(loggedInUser => {
              layoutModelItems.hasPortAccessPot.renderReady(userHasPortAccess => {
                if (userHasPortAccess) {
                  val airportConfigRCP = SPACircuit.connect(_.airportConfig)

                  airportConfigRCP(airportConfigMP => {
                    val airportConfig = airportConfigMP()
                    <.div(
                      airportConfig.renderReady(airportConfig => {
                        <.div(
                          Navbar(props.ctl, props.currentLoc.page, loggedInUser, airportConfig),
                          <.div(^.className := "container",
                            <.div(<.div(props.currentLoc.render()))
                          ), VersionUpdateNotice())
                      }), layoutModelItems
                        .displayAlertDialog
                        .renderReady(displayDialog => PortRestrictionsModalAlert(displayDialog, loggedInUser)))
                  })

                } else {
                  SPACircuit.dispatch(RedirectToRequestAccess)
                  <.div(RestrictedAccessByPortPage(loggedInUser, props.ctl))
                }
              })
            }))
        )
      })
    })
    .build

  def apply(ctl: RouterCtl[Loc], currentLoc: Resolution[Loc]): VdomElement = component(Props(ctl, currentLoc))
}
