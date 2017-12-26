package outwatch

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.{Cancelable, Scheduler}
import outwatch.dom._
import outwatch.dom.dsl.attributes.lifecycle

package object extras {
  type >-->[-I, +O] = Pipe[I, O]
  type <--<[+O, -I] = Pipe[I, O]


  def managed(subscription: IO[Cancelable])(implicit s: Scheduler): VDomModifier = {
    subscription.flatMap { sub: Cancelable =>
      lifecycle.onDestroy --> Sink.create(_ => IO {
        sub.cancel()
        Continue
      })
    }
  }

  def managed(sub1: IO[Cancelable], sub2: IO[Cancelable], subscriptions: IO[Cancelable]*)(implicit s: Scheduler): VDomModifier = {

    (sub1 :: sub2 :: subscriptions.toList).sequence.flatMap { subs: Seq[Cancelable] =>
      subs.map { sub =>
        lifecycle.onDestroy --> Sink.create(_ => IO {
          sub.cancel()
          Continue
        }
        )
      }
    }
  }
}
