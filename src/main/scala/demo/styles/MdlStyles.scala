package demo.styles

import org.scalajs.dom.Element
import outwatch.Sink
import outwatch.dom.Attributes
import outwatch.mdl.Mdl

import scala.scalajs.js
import scalacss.StyleSheet
import scalacss.DevDefaults._

/**
  * Created by marius on 23/05/17.
  */




trait MdlStyles extends Mdl { self: StyleSheet.Inline =>

  object mdl {

    import dsl._

    val textfield = mixin (
      addClassName("mdl-textfield mdl-js-textfield mdl-textfield--floating-label")
    )

    val textinput = mixin (
      addClassName("mdl-textfield__input")
    )

    val textlabel = mixin (
      addClassName("mdl-textfield__label")
    )

    val button = mixin (
      addClassName("mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect")
    )
  }
}



