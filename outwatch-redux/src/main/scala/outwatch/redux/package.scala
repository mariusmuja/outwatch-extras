package outwatch

import cats.functor.Contravariant

import scala.language.implicitConversions


/**
  * Created by marius on 25/06/17.
  */
package object redux {

  type Sink[-A] = outwatch.Sink[A]
  type Source[+A] = monix.reactive.Observable[A]

  trait >-->[-I, +O] {
    def sink: Sink[I]

    def source: Source[O]

    def map[O2](f: O => O2): I >--> O2 = HandlerPipe(sink, source.map(f))

    def contramap[I2](f: I2 => I): I2 >--> O = HandlerPipe(sink.redirectMap(f), source)

    def imap[I2, O2](f: I2 => I)(g: O => O2): I2 >--> O2 = HandlerPipe(sink.redirectMap(f), source.map(g))

    def contracollect[I2](f: PartialFunction[I2, I]): I2 >--> O = HandlerPipe(
      sink.redirect(_.collect(f)), source
    )

    def collect[O2](f: PartialFunction[O, O2]): I >--> O2 = HandlerPipe(sink, source.collect(f))

    def icollect[I2, O2](f: PartialFunction[I2, I])(g: PartialFunction[O, O2]): I2 >--> O2 = HandlerPipe(
      sink.redirect(_.collect(f)), source.collect(g)
    )
  }

  object >--> {
    implicit def toSink[I](store: >-->[I, _]): Sink[I] = store.sink
    implicit def toSource[O](store: >-->[_, O]): Source[O] = store.source
  }

  case class HandlerPipe[-I, +O](sink: Sink[I], source: Source[O]) extends >-->[I, O]
}
