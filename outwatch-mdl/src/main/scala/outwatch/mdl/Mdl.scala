package outwatch.mdl

import cats.effect.IO
import org.scalajs.dom
import outwatch.Sink
import outwatch.dom.{Attributes, InsertHook}

import scala.scalajs.js

/**
  * Created by marius on 11/06/17.
  */
trait Mdl {

  private val upgradeElement = Sink.create[dom.Element] { e =>
    val componentHandler = js.Dynamic.global.componentHandler
    IO(if (!js.isUndefined(componentHandler)) componentHandler.upgradeElement(e))
  }

  val material: InsertHook = Attributes.insert --> upgradeElement
}