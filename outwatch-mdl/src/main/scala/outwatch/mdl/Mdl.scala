package outwatch.mdl

import org.scalajs.dom.Element
import outwatch.Sink
import outwatch.dom.Attributes

import scala.scalajs.js
import scalacss.defaults.Exports.StyleSheet

/**
  * Created by marius on 11/06/17.
  */

trait Mdl { self: StyleSheet.Inline =>

  private val upgradeElement = Sink.create[Element] { e =>
    val componentHandler = js.Dynamic.global.componentHandler
    if (!js.isUndefined(componentHandler)) componentHandler.upgradeElement(e)
  }

  val material = Attributes.insert --> upgradeElement
}