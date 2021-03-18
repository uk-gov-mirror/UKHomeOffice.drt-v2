package drt.client.components.styles
import scalacss.internal.mutable.StyleSheet
import CssSettings._

case class FormFieldsStyle(common: CommonStyle = DefaultCommonStyle) extends StyleSheet.Inline {
  import common.theme
  import dsl._

  val container = style(
    display.flex,
    flexWrap.wrap
  )

  val textField = style(
    marginLeft(theme.spacing.unit.px),
    marginRight(theme.spacing.unit.px),
    width(280.px),
    unsafeChild("label")(
      fontSize(1.5.rem)
    )
  )

  val dense = style(
    marginTop(19.px)
  )

  val simulationStepper = style(
    margin(15.px),
    padding(15.px),
    minHeight(550.px)
  )

  val stepperButton = style(
    marginRight(10.px),
  )

  val simulationCharts = style(
    padding(15.px),
    margin(15.px)
  )

}

object DefaultFormFieldsStyle extends FormFieldsStyle
