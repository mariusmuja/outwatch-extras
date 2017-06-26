package outwatch

import outwatch.dom.Handlers

/**
  * Created by marius on 25/06/17.
  */
object SinkUtil {

  def redirectInto[A](sinks: Sink[A]*): Sink[A] = {
    val handler = Handlers.createHandler[A]()
    val subscriptions = sinks.map(sink => handler.subscribe(sink.observer))
    handler
  }
}
