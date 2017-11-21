package outwatch.redux

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import outwatch.dom.{Handler, Observable, Pipe}
import outwatch.extras.>-->

trait Effects[Effect, EffectResult] {

  def effects: Effect => Observable[EffectResult]

  val switch: IO[Effect >--> EffectResult] = Handler.create[Effect]().map { handler =>
    Pipe(handler, handler.switchMap(effects).share)
  }

  val merge: IO[Effect >--> EffectResult] = Handler.create[Effect]().map { handler =>
    Pipe(handler, handler.mergeMap(effects).share)
  }

  val concat: IO[Effect >--> EffectResult] = Handler.create[Effect]().map { handler =>
    Pipe(handler, handler.concatMap(effects).share)
  }

  def switchCollect[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[EffectResult, A]): IO[E >--> A] =
    switch.map(_.collectPipe(f)(g))

  def mergeCollect[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[EffectResult, A]): IO[E >--> A] =
    merge.map(_.collectPipe(f)(g))

  def concatCollect[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[EffectResult, A]): IO[E >--> A] =
    concat.map(_.collectPipe(f)(g))
}