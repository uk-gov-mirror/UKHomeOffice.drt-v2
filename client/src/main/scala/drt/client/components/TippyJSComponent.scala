package drt.client.components

import drt.client.components.styles.DefaultToolTipsStyle
import drt.client.logger.{Logger, LoggerFactory}
import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react.component.Js.{RawMounted, UnmountedWithRawType}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Children, JsComponent, Ref, ScalaComponent}
import org.scalajs.dom.raw.{HTMLElement, KeyboardEvent}
import org.scalajs.dom.{Event, document}
import scalacss.ScalaCssReactImplicits

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object TippyJSComponent {

  val log: Logger = LoggerFactory.getLogger("TippyJSComponent")

  @JSImport("@tippyjs/react", JSImport.Default)
  @js.native
  private object TippyReactRaw extends js.Object

  val elementRef: Simple[HTMLElement] = Ref[HTMLElement]

  @js.native
  trait Props extends js.Object {
    var content: js.Any = js.native
    var interactive: Boolean = js.native
    var reference: js.Object = js.native
    var theme: String = js.native
    var maxWidth: js.Any = js.native
    var trigger: String = js.native
    var placement: String = js.native
    var plugins: js.UndefOr[js.Array[js.Any]] = js.native
    var onTrigger: js.Function2[TippyElement, Event, Unit] = js.native
  }

  def props(content: js.Object, interactive: Boolean, plugins: js.Array[js.Any], triggerEvent: String): Props = {
    val p = (new js.Object).asInstanceOf[Props]

    p.interactive = interactive
    p.content = content
    p.theme = "light-border"
    p.maxWidth = "None"
    p.trigger = triggerEvent
    p.placement = "top-end"
    p.plugins = plugins

    p
  }

  val component = JsComponent[Props, Children.Varargs, Null](TippyReactRaw)

  def apply[A](
                content: js.Object,
                interactive: Boolean,
                trigger: VdomTagOf[HTMLElement],
                plugins: js.Array[js.Any] = js.Array(),
                triggerEvent: String = Tippy.TriggerEvents.focus
              ): UnmountedWithRawType[Props, Null, RawMounted[Props, Null]] =
    component(props(content, interactive, plugins, triggerEvent))(trigger)
}

@js.native
trait TippyElement extends js.Object {
  def hide(): Unit = js.native
}

class HideOnEscapeHooks(t: TippyElement) extends js.Object {

  def onShow(el: TippyElement): Unit = document
    .addEventListener("keydown", (event: KeyboardEvent) => {
      val escapeKeyCode = 27
      if (event.keyCode == escapeKeyCode) el.hide()
    })
}

class HideOnEsc() extends js.Object {

  def fn(h: TippyElement) = new HideOnEscapeHooks(h)

}

object Tippy extends ScalaCssReactImplicits {

  object TriggerEvents {
    val focus = "focus"
    val hover = "mouseenter"
    val focusAndHover = s"$hover $focus"
  }

  case class Props(content: VdomElement, interactive: Boolean, trigger: VdomNode, triggerEvent: String)

  val component = ScalaComponent.builder[Props]("FlightChart")
    .render_P(props => {

      val triggerWithTabIndex = <.span(
        ^.className := "tooltip-trigger",
        DefaultToolTipsStyle.triggerHoverIndicator,
        props.trigger,
        ^.tabIndex := 0
      )

      val plugins: js.Array[js.Any] = js.Array(new HideOnEsc())

      TippyJSComponent(props.content.rawElement, props.interactive, triggerWithTabIndex, plugins, props.triggerEvent)
    })
    .build

  def apply(content: VdomElement, interactive: Boolean, trigger: VdomNode, triggerEvent: String = TriggerEvents.focus) =
    component(Props(content, interactive, trigger, triggerEvent))

  def interactive(content: VdomElement, trigger: VdomNode) =
    apply(content, interactive = true, <.div(trigger))

  def describe(content: VdomElement, trigger: VdomNode) =
    apply(content, interactive = false, <.div(trigger))

  def interactiveInfo(content: VdomElement) =
    apply(content, interactive = true, Icon.infoCircle)

  def info(content: VdomElement) =
    apply(content, interactive = true, Icon.infoCircle)

  def info(content: String) =
    apply(<.div(content), interactive = true, Icon.infoCircle)

  def infoHover(content: String) =
    apply(<.div(content), interactive = true, Icon.infoCircle, TriggerEvents.focusAndHover)

}


