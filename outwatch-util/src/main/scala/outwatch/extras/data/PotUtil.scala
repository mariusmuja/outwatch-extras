package outwatch.extras.data

import monix.execution.misc.NonFatal
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}


trait PotUtil {

  implicit class FromFuture[T](f: => Future[T])(implicit ec: ExecutionContext) {

    def observablePot : Observable[Pot[T]] = Observable.fromFuture {
      f.map(Ready.apply).recover { case NonFatal(e) => Failed(e) }
    }

    def observablePot[B](g: Pot[T] => B): Observable[B] = observablePot.map(g)
  }

}