package outwatch.extras.data

import monix.reactive.Observable
import outwatch.all.VDomModifier

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


trait PotErrorRender {
  def render(e: Throwable): VDomModifier
}

trait PotPendingRender {
  def render(startTime: Long): VDomModifier
}

trait PotPendingStaleRender {
  def render(vnode: VDomModifier, startTime: Long): VDomModifier
}
object PotPendingStaleRender {
  implicit object default extends PotPendingStaleRender {
    def render(vnode: VDomModifier, startTime: Long): VDomModifier  = vnode
  }
}

trait PotEmptyRender {
  def render: VDomModifier
}


trait PotUtil {

  implicit class FromFuture[T](f: => Future[T])(implicit ec: ExecutionContext) {

    def observablePot : Observable[Pot[T]] = Observable.fromFuture {
      f.map(Ready.apply).recover { case NonFatal(e) => Failed(e) }
    }

    def observablePot[B](g: Pot[T] => B): Observable[B] = observablePot.map(g)
  }

}