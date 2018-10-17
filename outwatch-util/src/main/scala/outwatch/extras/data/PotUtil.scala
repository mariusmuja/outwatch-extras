package outwatch.extras.data

import monix.execution.misc.NonFatal
import monix.reactive.Observable
import outwatch.dom.VNode

import scala.concurrent.{ExecutionContext, Future}


trait PotErrorRender {
  def render(e: Throwable): VNode
}

trait PotPendingRender {
  def render(startTime: Long): VNode
}

trait PotPendingStaleRender {
  def render(vnode: VNode, startTime: Long): VNode
}
object PotPendingStaleRender {
  implicit object default extends PotPendingStaleRender {
    def render(vnode: VNode, startTime: Long): VNode  = vnode
  }
}

trait PotEmptyRender {
  def render: VNode
}


trait PotUtil {

  implicit class FromFuture[T](f: => Future[T])(implicit ec: ExecutionContext) {

    def observablePot : Observable[Pot[T]] = Observable.fromFuture {
      f.map(Ready.apply).recover { case NonFatal(e) => Failed(e) }
    }

    def observablePot[B](g: Pot[T] => B): Observable[B] = observablePot.map(g)
  }

}