package drt.client.components

import diode.UseValueEq
import drt.client.SPAMain
import drt.client.SPAMain._
import drt.client.components.Icon._
import drt.client.services.JSDateConversions.SDate
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared._
import japgolly.scalajs.react.component.Js
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CtorType, _}
import org.scalajs.dom.html.{Div, LI}
import uk.gov.homeoffice.drt.auth.LoggedInUser
import uk.gov.homeoffice.drt.auth.Roles._

object MainMenu {
  @inline private def bss: BootstrapStyles.type = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[Loc],
                   currentLoc: Loc,
                   feeds: Seq[FeedSourceStatuses],
                   airportConfig: AirportConfig,
                   user: LoggedInUser) extends UseValueEq

  case class MenuItem(idx: Int, label: Props => VdomNode, icon: Icon, location: Loc, classes: List[String] = List())

  val dashboardMenuItem: MenuItem = MenuItem(0, _ => "Dashboard", Icon.dashboard, UserDashboardLoc)

  def usersMenuItem(position: Int): MenuItem = MenuItem(position, _ => "Users", Icon.users, KeyCloakUsersLoc)

  def alertsMenuItem(position: Int): MenuItem = MenuItem(position, _ => "Alerts", Icon.briefcase, AlertLoc, List("alerts-link"))

  def statusMenuItem(position: Int, feeds: Seq[FeedSourceStatuses]): MenuItem = MenuItem(position, _ => s"Feeds", Icon.barChart, StatusLoc, List(feedsRag(feeds)))

  val portConfigMenuItem: Int => MenuItem = (position: Int) => MenuItem(position, _ => s"Port Config", Icon.cogs, PortConfigLoc)

  val forecastUploadFile: Int => MenuItem = (position: Int) => MenuItem(position, _ => "Forecast Upload", Icon.upload, ForecastFileUploadLoc)

  def feedsRag(feeds: Seq[FeedSourceStatuses]): String = {
    val statuses = feeds.map(f => FeedStatuses.ragStatus(SDate.now().millisSinceEpoch, f.feedSource.maybeLastUpdateThreshold, f.feedStatuses))
    val rag = if (statuses.contains(Red)) Red
    else if (statuses.contains(Amber)) Amber
    else Green

    rag.toString
  }

  def menuItems(airportConfig: AirportConfig, currentLoc: Loc, userRoles: Set[Role], feeds: Seq[FeedSourceStatuses]): List[MenuItem] = {
    val addFileUpload: List[(Role, Int => MenuItem)] =
      if (List(PortCode("LHR"), PortCode("LGW"), PortCode("STN"), PortCode("TEST")) contains airportConfig.portCode)
        List((PortFeedUpload, forecastUploadFile))
      else List.empty

    def terminalDepsMenuItem: List[(Role, Int => MenuItem)] = airportConfig.terminals.map { tn =>
      val terminalName = tn.toString
      val targetLoc = currentLoc match {
        case tptl: TerminalPageTabLoc if tptl.mode == "dashboard" =>
          TerminalPageTabLoc(terminalName, tptl.mode, tptl.subMode)
        case tptl: TerminalPageTabLoc =>
          TerminalPageTabLoc(terminalName, tptl.mode, tptl.subMode,
            tptl.withUrlParameters(UrlDateParameter(tptl.date),
              UrlTimeRangeStart(tptl.timeRangeStartString),
              UrlTimeRangeEnd(tptl.timeRangeEndString)).queryParams)
        case _ => TerminalPageTabLoc(terminalName)
      }
      (BorderForceStaff, (offset: Int) => MenuItem(offset, _ => terminalName, Icon.calculator, targetLoc))
    }.toList

    val restrictedMenuItems: List[(Role, Int => MenuItem)] = List(
      (ManageUsers, usersMenuItem _),
      (CreateAlerts, alertsMenuItem _),
    ) ++ addFileUpload ++ terminalDepsMenuItem :+ ((ViewConfig, portConfigMenuItem))

    val nonTerminalUnrestrictedMenuItems = dashboardMenuItem :: Nil

    val itemsForLoggedInUser: List[MenuItem] = restrictedMenuItemsForRole(restrictedMenuItems, userRoles, nonTerminalUnrestrictedMenuItems.length)

    val nonTerminalMenuItems = nonTerminalUnrestrictedMenuItems ::: itemsForLoggedInUser
    nonTerminalMenuItems :+ statusMenuItem(nonTerminalMenuItems.length + airportConfig.terminals.size, feeds)
  }

  def restrictedMenuItemsForRole(restrictedMenuItems: List[(Role, Int => MenuItem)], roles: Set[Role], startIndex: Int): List[MenuItem] = {
    val itemsForLoggedInUser = restrictedMenuItems.collect {
      case (role, menuItemCallback) if roles.contains(role) => menuItemCallback
    }.zipWithIndex.map {
      case (menuItemCallback, index) =>
        menuItemCallback(startIndex + index)
    }
    itemsForLoggedInUser
  }

  def lastUpdatedDescription(maybeLastUpdated: Option[MillisSinceEpoch]): String = maybeLastUpdated.map(lastUpdated => {
    val secondsAgo = (SDate.now().millisSinceEpoch - lastUpdated) / 1000
    val minutesAgo = secondsAgo / 60
    if (minutesAgo > 1) s"$minutesAgo mins" else if (minutesAgo == 1) s"$minutesAgo min" else "< 1 min"
  }).getOrElse("n/a")

  private class Backend() {
    def render(props: Props): VdomTagOf[Div] = {
      val children: Seq[TagOf[LI]] = for (item <- menuItems(props.airportConfig, props.currentLoc, props.user.roles, props.feeds)) yield {
        val active = (props.currentLoc, item.location) match {
          case (TerminalPageTabLoc(tn, _, _, _), TerminalPageTabLoc(tni, _, _, _)) => tn == tni
          case (current, itemLoc) => current == itemLoc
        }
        val classes = List(("active", active))
        <.li(^.key := item.idx, ^.classSet(classes: _*), ^.className := item.classes.mkString(" "),
          props.router.link(item.location)(item.icon, " ", item.label(props))
        )
      }

      val navItems: Seq[VdomTagOf[LI]] = if (props.user.portRoles.size > 1)
        children :+ <.li(PortSwitcher(props.user, props.airportConfig.portCode))
      else
        children
      <.div(
        <.ul(^.classSet(bss.navbarClsSet.map(cn => (cn, true)): _*), ^.className := "main-menu")(navItems.toTagMod)
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("MainMenu")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[Loc],
            currentLoc: Loc,
            feeds: Seq[FeedSourceStatuses],
            airportConfig: AirportConfig,
            user: LoggedInUser): VdomElement
  = component(Props(ctl, currentLoc, feeds, airportConfig, user))
}

object PortSwitcher {
  case class Props(user: LoggedInUser, portCode: PortCode) extends UseValueEq

  case class State(showDropDown: Boolean = false)

  val component: Js.ComponentSimple[Props, CtorType.Props, Unmounted[Props, State, Unit]] =
    ScalaComponent.builder[Props]("PortSwitcher")
      .initialState(State())
      .renderPS((scope, props, state) => {
        val showClass = if (state.showDropDown) "show" else ""
        val otherPorts = props.user.portRoles.collect {
          case portRole: PortAccess if props.portCode != PortCode(portRole.name) => PortCode(portRole.name)
        }
        if (otherPorts.size == 1) {
          <.a(Icon.plane, " ", ^.href := SPAMain.urls.urlForPort(otherPorts.head.toString), otherPorts.head.iata)
        } else {
          <.span(
            ^.className := "dropdown",
            <.a(Icon.plane, " ", "Switch port"),
            ^.onClick --> scope.modState(_.copy(showDropDown = !state.showDropDown)),
            if (state.showDropDown) <.div(^.className := "menu-overlay", ^.onClick --> scope.modState(_ => State())) else "",
            <.ul(^.className := s"main-menu__port-switcher dropdown-menu $showClass",
              otherPorts.toList.sorted.map(p => <.li(^.className := "dropdown-item",
                <.a(^.href := SPAMain.urls.urlForPort(p.toString), p.iata))).toTagMod
            )
          )
        }
      }).build

  def apply(user: LoggedInUser, portCode: PortCode): VdomElement = component(Props(user, portCode))
}
