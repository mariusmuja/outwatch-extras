package outwatch

package object extras {
  type >-->[-I, +O] = Pipe[I, O]
  type <--<[+O, -I] = Pipe[I, O]
}
