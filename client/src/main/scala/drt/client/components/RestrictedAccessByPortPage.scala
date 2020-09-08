package drt.client.components

import drt.auth.{LoggedInUser, Role}
import drt.client.SPAMain.Loc
import drt.client.logger.{Logger, LoggerFactory}
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.{AirportConfig, AirportConfigs, PortCode}
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^.{^, _}
import japgolly.scalajs.react.{CtorType, ScalaComponent}
import org.scalajs.dom

object RestrictedAccessByPortPage {
  val allAirportConfigsToDisplay: List[AirportConfig] = AirportConfigs.allPortConfigs diff AirportConfigs.testPorts
  val allPorts: List[PortCode] = AirportConfigs.allPortConfigs.map(config => config.portCode)
  val urlLowerCase: String = dom.document.URL.toLowerCase
  val portRequested: PortCode = allPorts
    .find(port => urlLowerCase.contains(s"${port.toString.toLowerCase}"))
    .getOrElse(PortCode("InvalidPortCode"))

  def allPortsAccessible(roles: Set[Role]): Set[PortCode] = AirportConfigs.allPortConfigs
    .filter(airportConfig => roles.contains(airportConfig.role)).map(_.portCode).toSet

  def userCanAccessPort(loggedInUser: LoggedInUser, portCode: PortCode): Boolean = AirportConfigs.
    allPortConfigs
    .find(_.portCode == portCode)
    .exists(c => loggedInUser.hasRole(c.role))

  case class Props(loggedInUser: LoggedInUser, ctl: RouterCtl[Loc])

  val log: Logger = LoggerFactory.getLogger(getClass.getName)

  case class State(title: Option[String] = None, message: Option[String] = None, expiryDateTime: Option[MillisSinceEpoch] = None)

  def url(port: PortCode): String = urlLowerCase.replace(portRequested.toString.toLowerCase, port.toString.toLowerCase)

  val component: Component[Props, Unit, Unit, CtorType.Props] = ScalaComponent.builder[Props]("RestrictedAccessForPort")
    .render_P(props => {
      <.div(^.className := "access-restricted",
        <.span(
          <.h2(^.id := "access-restricted", "Redirecting"),
          <.div(
            <.p(^.id := "email-for-access", "Please wait whilst we redirect you")
          )
        )
      )
    })
    .build

  def apply(loggedInUser: LoggedInUser, ctl: RouterCtl[Loc] ): VdomElement = component(Props(loggedInUser, ctl))
}
