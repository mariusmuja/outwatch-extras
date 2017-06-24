package outwatch

import rxscalajs.Observable

import scala.language.implicitConversions

/**
  * Created by marius on 16/06/17.
  */
package object extras {

  case class Handler[T](source: Observable[T], sink: Sink[T]) {

    def redirect(projection: Observable[T] => Observable[T]): Handler[T] = {
      val newSink = sink.redirect[T](projection)
      Handler(source, newSink)
    }
  }

  object Handler {

    implicit def toSink[T](handler: Handler[T]): Sink[T] = handler.sink
    implicit def toSource[T](handler: Handler[T]): Observable[T] = handler.source
    implicit def fromSinkObservable[T](handler: Sink[T] with Observable[T]): Handler[T] = apply(handler)

    def apply[T](handler: Observable[T] with Sink[T]): Handler[T] = Handler(handler, handler)
  }

}
