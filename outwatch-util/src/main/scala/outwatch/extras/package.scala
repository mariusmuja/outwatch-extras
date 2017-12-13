package outwatch

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Cancelable
import outwatch.dom.VDomModifier
import outwatch.dom.OutwatchAttributes

package object extras {
  type >-->[-I, +O] = Pipe[I, O]
  type <--<[+O, -I] = Pipe[I, O]


  def managed(subscription: IO[Cancelable]): VDomModifier = {
    subscription.flatMap { sub: Cancelable =>
      OutwatchAttributes.onDestroy --> Sink.create(_ => IO {
        sub.cancel()
        Continue
      })
    }
  }

  def managed(sub1: IO[Cancelable], sub2: IO[Cancelable], subscriptions: IO[Cancelable]*): VDomModifier = {
    import cats.instances.list._
    import cats.syntax.traverse._

    (sub1 :: sub2 :: subscriptions.toList).sequence.flatMap { subs: List[Cancelable] =>
      subs.map { sub =>
        OutwatchAttributes.onDestroy --> Sink.create(_ => IO {
          sub.cancel()
          Continue
        })
      }
    }
  }
}
