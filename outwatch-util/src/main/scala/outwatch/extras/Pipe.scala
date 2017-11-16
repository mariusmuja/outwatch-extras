package outwatch.extras

import monix.reactive.Observable
import outwatch.Sink

import scala.language.implicitConversions

trait Pipe[-I, +O] {
  def sink: Sink[I]

  def source: Observable[O]

  def mapSink[I2](f: I2 => I): Pipe[I2, O] = Pipe(sink.redirectMap(f), source)

  def mapSource[O2](f: O => O2): Pipe[I, O2] = Pipe(sink, source.map(f))

  def mapPipe[I2, O2](f: I2 => I)(g: O => O2): Pipe[I2, O2] = Pipe(sink.redirectMap(f), source.map(g))


  def collectSink[I2](f: PartialFunction[I2, I]): Pipe[I2, O] = Pipe(sink.redirect(_.collect(f)), source)

  def collectSource[O2](f: PartialFunction[O, O2]): Pipe[I, O2] = Pipe(sink, source.collect(f))

  def collectPipe[I2, O2](f: PartialFunction[I2, I])(g: PartialFunction[O, O2]): Pipe[I2, O2] = Pipe(
    sink.redirect(_.collect(f)), source.collect(g)
  )


  def transformSink[I2](f: Observable[I2] => Observable[I]): Pipe[I2, O] = Pipe(sink.redirect(f), source)

  def transformSource[O2](f: Observable[O] => Observable[O2]): Pipe[I, O2] = Pipe(sink, f(source))

  def transformPipe[I2, O2](f: Observable[I2] => Observable[I])(g: Observable[O] => Observable[O2]): Pipe[I2, O2] =
    Pipe(sink.redirect(f), g(source))


  def filterSource(f: O => Boolean): Pipe[I, O] = Pipe(sink, source.filter(f))
}

object Pipe {
  implicit def toSink[I](store: Pipe[I, _]): Sink[I] = store.sink

  implicit def toSource[O](store: Pipe[_, O]): Observable[O] = store.source

  def apply[I, O](_sink: Sink[I], _source: Observable[O]): Pipe[I, O] = new Pipe[I, O] {
    def sink: Sink[I] = _sink

    def source: Observable[O] = _source
  }

  def apply[A](handler: outwatch.dom.Handler[A]): Pipe[A, A] = Pipe(handler, handler)

  implicit class FilterSink[I, +O](pipe: Pipe[I, O]) {
    def filterSink(f: I => Boolean): Pipe[I, O] = Pipe(pipe.sink.redirect(_.filter(f)), pipe.source)
  }

}