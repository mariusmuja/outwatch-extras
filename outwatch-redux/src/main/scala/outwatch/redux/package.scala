package outwatch

/**
  * Created by marius on 25/06/17.
  */
package object redux {

  type Sink[-A] = outwatch.Sink[A]
  type Source[+A] = monix.reactive.Observable[A]

  type >-->[-I, +O] = Pipe[I, O]
  val >--> = Pipe
}
