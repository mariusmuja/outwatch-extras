package outwatch.mdl

import cats.effect.IO
import monix.execution.Ack.Continue
import org.scalajs.dom
import outwatch.Sink
import outwatch.dom.{Attributes, InsertHook}

import scala.scalajs.js

/**
  * Created by marius on 11/06/17.
  */
trait Mdl {

  private val upgradeElement = Sink.create[dom.Element] { e =>
    IO {
      val componentHandler = js.Dynamic.global.componentHandler
      if (!js.isUndefined(componentHandler)) componentHandler.upgradeElement(e)
      Continue
    }
  }

  val material: IO[InsertHook] = Attributes.insert --> upgradeElement
}