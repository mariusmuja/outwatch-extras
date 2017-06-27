package outwatch.extras

import outwatch.Sink
import outwatch.dom.Handlers

/**
  * Created by marius on 25/06/17.
  */
object SinkUtil {

  def redirectFrom[A](sinks: Sink[A]*): Sink[A] = {
    val handler = Handlers.createHandler[A]()
    val subscriptions = sinks.map(sink => sink <-- handler)
    handler
  }
}
