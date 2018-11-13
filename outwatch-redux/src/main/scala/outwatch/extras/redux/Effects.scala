package outwatch.extras.redux

import monix.execution.Scheduler.Implicits.global
import outwatch.Handler
import outwatch.dom.{IO, Observable}
import outwatch.extras.>-->

trait EffectLike {
  type Effect
  type Result

  def effects: Effect => Observable[Result]
}

trait EffectsOps { self: EffectLike =>
  val switch: IO[Effect >--> Result] = Handler.create[Effect].map { handler =>
    handler.transformSource(_.switchMap(effects).share)
  }

  val merge: IO[Effect >--> Result] = Handler.create[Effect].map { handler =>
    handler.transformSource(_.mergeMap(effects).share)
  }

  val concat: IO[Effect >--> Result] = Handler.create[Effect].map { handler =>
    handler.transformSource(_.concatMap(effects).share)
  }


  def switch[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[Result, A]): IO[E >--> A] =
    switch.map(_.collectPipe(f)(g))

  def merge[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[Result, A]): IO[E >--> A] =
    merge.map(_.collectPipe(f)(g))

  def concat[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[Result, A]): IO[E >--> A] =
    concat.map(_.collectPipe(f)(g))


  def switch[A](g: PartialFunction[Result, A]): IO[Effect >--> A] =
    switch.map(_.collectSource(g))

  def merge[A](g: PartialFunction[Result, A]): IO[Effect >--> A] =
    merge.map(_.collectSource(g))

  def concat[E, A](g: PartialFunction[Result, A]): IO[Effect >--> A] =
    concat.map(_.collectSource(g))
}

trait EffectsHandler extends EffectLike with EffectsOps

trait Effects[E, ER] extends EffectsHandler {
  type Effect = E
  type Result = ER
}