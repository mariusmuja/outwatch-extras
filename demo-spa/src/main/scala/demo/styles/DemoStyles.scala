package demo.styles

import monix.execution.Scheduler
import outwatch.styles.{ComponentStyle, Styles}

import scalacss.DevDefaults._

trait GlobalScheduler {
  def scheduler = Scheduler.global
}

trait LogAreaStyle extends ComponentStyle {

  class Style extends StyleSheet with MdlStyles with GlobalScheduler {

    import dsl._

    val textfield = style(
      mdl.textfield,
      height(400.px),
      width(400.px).important,
      fontFamily :=! "Courier New",
      fontSize(14.px).important
    )
  }

  object Style {
    implicit object default extends Style with Styles.Publish
  }
}



trait TextFieldStyle extends ComponentStyle {

  class Style extends StyleSheet with MdlStyles with GlobalScheduler {

    import dsl._

    val textfield = style (
      mdl.textfield,
      marginRight(8.px).important
    )

    val textinput = style (
      mdl.textinput
    )

    val textlabel = style (
      mdl.textlabel
    )

    val button = style (
      mdl.button
    )
  }

  object Style {
    implicit object default extends Style with Styles.Publish
  }
}

trait TodoModuleStyle extends ComponentStyle {

  class Style extends StyleSheet with MdlStyles with GlobalScheduler {

    import dsl._

    val textinput = style(
      mdl.textinput
    )

    val textlabel = style(
      mdl.textlabel
    )

    val button = style(
      mdl.button,
      marginLeft(8.px)
    )
  }

  object Style {
    implicit object default extends Style with Styles.Publish
  }
}
