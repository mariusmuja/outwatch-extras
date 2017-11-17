package outwatch.mdl

import cats.effect.IO
import monix.execution.Ack.Continue
import org.scalajs.dom
import outwatch.Sink
import outwatch.dom.{Attributes, VDomModifier}

import scala.scalajs.js

/**
  * Created by marius on 11/06/17.
  */
trait Mdl {

  private def componentHandler = js.Dynamic.global.componentHandler

  private val upgradeInsertedElement = Sink.create[dom.Element] { e =>
    IO {
      if (!js.isUndefined(componentHandler)) componentHandler.upgradeElement(e)
      Continue
    }
  }

  private val upgradePostPatchElement = Sink.create[(dom.Element, dom.Element)] { case (_, e) =>
    IO {
      e.removeAttribute("data-upgraded")
      if (!js.isUndefined(componentHandler)) componentHandler.upgradeElement(e)
      Continue
    }
  }

  val material: VDomModifier = Seq(
    Attributes.insert --> upgradeInsertedElement,
    Attributes.postpatch --> upgradePostPatchElement
  )

  
  def material(id: String): VDomModifier = {
    val hook = Sink.create[(dom.Element, dom.Element)] { _ =>
      IO {
        Option(dom.document.getElementById(id)).foreach { e =>
          e.removeAttribute("data-upgraded")
          if (!js.isUndefined(componentHandler)) componentHandler.upgradeElement(e)
        }
        Continue
      }
    }
    Attributes.postpatch --> hook
  }

}