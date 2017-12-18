package outwatch.extras.mdl

import cats.effect.IO
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
    IO(updateElement(e)).map(_ => Continue)
  }

  private val postpatchHook = Sink.create[(dom.Element, dom.Element)] { case (_, e) =>
    IO(updateElement(e)).map(_ => Continue)
  }

  val material: VDomModifier = Seq(onInsert --> insertHook, onPostpatch --> postpatchHook)

  def material(id: String): VDomModifier = {

    val update = IO {
      Option(dom.document.getElementById(id)).foreach(updateElement)
    }.map(_ => Continue)

    val insertHook = Sink.create[dom.Element]( _ => update )
    val postpatchHook = Sink.create[(dom.Element, dom.Element)]( _ => update )

    Seq(onInsert --> insertHook, onPostpatch --> postpatchHook)
  }

}