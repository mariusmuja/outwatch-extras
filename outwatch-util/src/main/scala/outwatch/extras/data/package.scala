package outwatch.extras

import cats.{Eq, MonadError}
import cats.syntax.all._
import outwatch.AsVDomModifier
import outwatch.dom.VDomModifier

import scala.language.higherKinds
import scala.util.control.NonFatal

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


  type MonadThwableError[F[_]] = MonadError[F, Throwable]

  implicit class MonadErrorRecoverPot[T, F[_]: MonadThwableError](private val f: F[T]) {
    def recoverPot: F[Pot[T]] = f.map[Pot[T]](Ready.apply).recover { case NonFatal(e) => Failed(e) }

    def recoverPot[B](g: Pot[T] => B): F[B] = f.map[B](x => g(Ready(x))).recover { case NonFatal(e) => g(Failed(e)) }
  }


}
