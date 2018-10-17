package outwatch.extras

import outwatch.dom.VNode

package object data {

  import outwatch.dom.dsl.div

  implicit class PotSeqRender[T](val pot: Pot[Seq[T]]) extends AnyVal {
    def renderMap[M](renderer: T => M)(implicit conv: M => VNode,
      pendingRender: PotPendingRender,
      pendingStaleRender: PotPendingStaleRender,
      errorRender: PotErrorRender,
      emptyRender: PotEmptyRender
    ): Seq[VNode] = pot match {
      case Ready(res) => res.map(elem => conv(renderer(elem)))
      case PendingStale(res, start) => res.map(elem => pendingStaleRender.render(conv(renderer(elem)), start))
      case Pending(start) => Seq(pendingRender.render(start))
      case Failed(exception) => Seq(errorRender.render(exception))
      case Empty => Seq(emptyRender.render)
      case _ => Seq(div())
    }
  }

  implicit class PotRender[T](val pot: Pot[T]) extends AnyVal {

    def render[M](renderer: T => M)(implicit conv: M => VNode,
      pendingRender: PotPendingRender,
      pendingStaleRender: PotPendingStaleRender,
      errorRender: PotErrorRender,
      emptyRender: PotEmptyRender
    ): VNode = pot match {
      case Ready(elem) => conv(renderer(elem))
      case PendingStale(elem, start) => pendingStaleRender.render(conv(renderer(elem)), start)
      case Pending(start) => pendingRender.render(start)
      case Failed(exception) => errorRender.render(exception)
      case Empty => emptyRender.render
      case _ => div()
    }

    def renderSeq[M](renderer: T => M)(implicit conv: M => Seq[VNode],
      pendingRender: PotPendingRender,
      pendingStaleRender: PotPendingStaleRender,
      errorRender: PotErrorRender,
      emptyRender: PotEmptyRender
    ): Seq[VNode] = pot match {
      case Ready(elem) => conv(renderer(elem))
      case PendingStale(elem, start) => Seq(pendingStaleRender.render(div(conv(renderer(elem)) : _*), start))
      case Pending(start) => Seq(pendingRender.render(start))
      case Failed(exception) => Seq(errorRender.render(exception))
      case Empty => Seq(emptyRender.render)
      case _ => Seq(div())
    }
  }


}
