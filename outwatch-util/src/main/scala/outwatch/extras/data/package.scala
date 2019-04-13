package outwatch.extras

import cats.Eq
import outwatch.AsVDomModifier
import outwatch.dom.VDomModifier

package object data {

  import outwatch.dom.dsl.div

  implicit class PotRender[T](val pot: Pot[T]) extends AnyVal {

    def render[M: AsVDomModifier](renderer: T => M)(implicit
      pendingRender: PotPendingRender,
      pendingStaleRender: PotPendingStaleRender,
      errorRender: PotErrorRender,
      emptyRender: PotEmptyRender
    ): VDomModifier = pot match {
      case Ready(elem) => renderer(elem)
      case PendingStale(elem, start) => pendingStaleRender.render(renderer(elem), start)
      case Pending(start) => pendingRender.render(start)
      case Failed(exception) => errorRender.render(exception)
      case Empty => emptyRender.render
      case _ => div()
    }

  }

  implicit def potEq[T]: Eq[Pot[T]] = Eq.instance { (x, y) =>
    x.state == y.state && x.toOption == y.toOption
  }


}
