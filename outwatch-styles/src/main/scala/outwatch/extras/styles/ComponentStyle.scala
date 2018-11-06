package outwatch.extras.styles

import outwatch.dom.VDomModifier
import outwatch.dom.dsl.attributes
import scalacss.defaults.Exports.StyleA

import scala.language.implicitConversions

/**
  * Created by marius on 23/05/17.
  */
trait ComponentStyle {

  type StyleSheet = scalacss.defaults.Exports.StyleSheet.Inline

  type Style <: StyleSheet

  implicit def styleToAttr(styleA: StyleA): VDomModifier = {
    attributes.className := styleA.htmlClass
  }

}
