package outwatch

import rxscalajs.Observable
import rxscalajs.subscription.AnonymousSubscription

import scala.language.implicitConversions

/**
  * Created by marius on 16/06/17.
  */
package object extras {

  case class Handler[T](
    source: Observable[T],
    sink: Sink[T]
  ) {
    def pipeInto(obs: Observable[T]) : Handler[T] = {
      val newSink = sink.redirect[T](sinkObs => sinkObs.merge(obs))
      Handler(source, newSink)
    }
  }

  object Handler {

    implicit def toSink[T](handler: Handler[T]): Sink[T] = handler.sink
    implicit def toSource[T](handler: Handler[T]): Observable[T] = handler.source

    def apply[T](handler: Observable[T] with Sink[T]): Handler[T] = Handler(handler, handler)
  }

  case class SinkPipe() {
    private var sub : Option[AnonymousSubscription] = None

    def pipe[T](obs: Observable[T], sink: Sink[T]) = {
      sub.foreach(_.unsubscribe())
      sub = Option(sink <-- obs)
    }
  }

}
