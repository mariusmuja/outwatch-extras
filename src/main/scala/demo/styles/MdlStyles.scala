package demo.styles

import scalacss.StyleSheet
import scalacss.DevDefaults._

/**
  * Created by marius on 23/05/17.
  */

object MdlStyles extends StyleSheet.Inline {

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