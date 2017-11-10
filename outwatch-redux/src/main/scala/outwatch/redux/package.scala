package outwatch

import scala.language.implicitConversions


/**
  * Created by marius on 25/06/17.
  */
package object redux {

  type Sink[-A] = outwatch.Sink[A]
  type Source[+A] = monix.reactive.Observable[A]

  trait Pipe[-I, +O] {
    def sink: Sink[I]

    def source: Source[O]

    def contraMap[I2](f: I2 => I): Pipe[I2, O] = HandlerPipe(sink.redirectMap(f), source)

    def map[O2](f: O => O2): Pipe[I, O2] = HandlerPipe(sink, source.map(f))

    def contraCollect[I2](f: PartialFunction[I2, I]): Pipe[I2, O] = HandlerPipe(
      sink.redirect(_.collect(f)), source
    )

    def collect[O2](f: PartialFunction[O, O2]): Pipe[I, O2] = HandlerPipe(sink, source.collect(f))

    def transformMap[I2, O2](f: I2 => I)(g: O => O2): Pipe[I2, O2] = HandlerPipe(sink.redirectMap(f), source.map(g))

    def transformCollect[I2, O2](f: PartialFunction[I2, I])(g: PartialFunction[O, O2]): Pipe[I2, O2] = HandlerPipe(
      sink.redirect(_.collect(f)), source.collect(g)
    )
  }

  object Pipe {
    implicit def toSink[I](store: Pipe[I, _]): Sink[I] = store.sink
    implicit def toSource[O](store: Pipe[_, O]): Source[O] = store.source
  }

  type >-->[-I, +O] = Pipe[I, O]
  val >--> = Pipe

  case class HandlerPipe[-I, +O](sink: Sink[I], source: Source[O]) extends Pipe[I, O]
}
