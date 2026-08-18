package drt.client.components

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

object NotFoundPage {
  case class Props()

  val component = ScalaComponent.builder[Props]("NotFoundPage")
    .render { _ =>
      <.h1("Page not found")
    }
    .build

  def apply(): VdomElement = component(Props())
}
