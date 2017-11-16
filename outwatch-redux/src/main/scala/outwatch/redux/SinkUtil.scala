package outwatch.redux

import cats.effect.IO
import outwatch.dom.Handlers

/**
  * Created by marius on 25/06/17.
  */
object SinkUtil {

  def redirectInto[T](sink1: Sink[T], sink2: Sink[T], otherSinks: Sink[T]*): IO[Sink[T]] = {
    Handlers.createHandler[T]().flatMap{ handler =>
      val sinks = Seq(sink1, sink2) ++ otherSinks
      sinks.map(_ <-- handler).reduce((a, b) => a.flatMap(_ => b)).map(_ => handler)
    }
  }
}
