package demo.styles

import outwatch.extras.mdl.Mdl

import scalacss.defaults.Exports._
import scalacss.internal.Compose

/**
  * Created by marius on 23/05/17.
  */
trait MdlStyles extends Mdl { self: StyleSheet.Inline =>

  object mdl {
    import dsl._

    def textfield(implicit c: Compose) = mixin (
      addClassName("mdl-textfield mdl-js-textfield mdl-textfield--floating-label")
    )

    def textinput(implicit c: Compose) = mixin (
      addClassName("mdl-textfield__input")
    )

    def textlabel(implicit c: Compose) = mixin (
      addClassName("mdl-textfield__label")
    )

    def button(implicit c: Compose)= mixin (
      addClassName("mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect")
    )
  }
}



