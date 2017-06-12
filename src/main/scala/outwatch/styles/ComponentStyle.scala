package outwatch.styles

import outwatch.dom.{Attribute, Attributes}

import scala.language.implicitConversions
import scalacss.defaults.Exports.{StyleA, StyleSheet}

/**
  * Created by marius on 23/05/17.
  */
trait ComponentStyle {
  type Style <: StyleSheet.Inline

  val defaultStyle: Style

  implicit def styleToAttr(styleA: StyleA): Attribute = {
    Attributes.`class` := styleA.htmlClass
  }
}
