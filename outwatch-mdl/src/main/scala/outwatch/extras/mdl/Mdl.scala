package outwatch.extras.mdl

import monix.execution.Ack.Continue
import monix.execution.Scheduler
import org.scalajs.dom
import outwatch.Sink
import outwatch.dom.VDomModifier
import outwatch.dom.dsl.attributes.lifecycle._

import scala.scalajs.js

/**
  * Created by marius on 11/06/17.
  */
trait Mdl {

  implicit def scheduler: Scheduler

  private def componentHandler = js.Dynamic.global.componentHandler

  private def updateElement(e: dom.Element): Unit = {
    e.removeAttribute("data-upgraded")
    if (!js.isUndefined(componentHandler)) componentHandler.upgradeElement(e).asInstanceOf[Unit]
  }

  private val insertHook = Sink.create[dom.Element] { e =>
    updateElement(e)
    Continue
  }

  private val postPatchHook = Sink.create[(dom.Element, dom.Element)] { case (_, e) =>
    updateElement(e)
    Continue
  }

  val material: VDomModifier = VDomModifier(
    insertHook.flatMap(sink => onInsert --> sink),
    postPatchHook.flatMap(sink => onPostPatch --> sink)
  )

  def material(id: String): VDomModifier = {

    val update = () => {
      Option(dom.document.getElementById(id)).foreach(updateElement)
      Continue
    }

    val insertHook = Sink.create[dom.Element](_ => update())
    val postPatchHook = Sink.create[(dom.Element, dom.Element)](_ => update() )

    VDomModifier(
      insertHook.flatMap(sink => onInsert --> sink),
      postPatchHook.flatMap(sink => onPostPatch --> sink)
    )
  }

}