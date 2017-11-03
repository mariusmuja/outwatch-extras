package outwatch

/**
  * Created by marius on 25/06/17.
  */
package object redux {

  type Sink[-A] = outwatch.Sink[A]
  type Source[+A] = rxscalajs.Observable[A]

  trait Handler[-I, +O] {
    def sink: Sink[I]

    def source: Source[O]

    def contramap[I2](f: I2 => I): Handler[I2, O] = HandlerPipe(sink.redirectMap(f), source)

    def map[O2](f: O => O2): Handler[I, O2] = HandlerPipe(sink, source.map(f))
  }

  case class HandlerPipe[-I, +O](sink: Sink[I], source: Source[O]) extends Handler[I, O]
}
