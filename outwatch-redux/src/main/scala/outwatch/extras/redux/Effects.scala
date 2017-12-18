package outwatch.extras.redux

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import outwatch.dom.{Handler, Observable}
import outwatch.extras.>-->

trait EffectsOps {
  type Effect
  type EffectResult

  def effects: Effect => Observable[EffectResult]

  val switch: IO[Effect >--> EffectResult] = Handler.create[Effect].map { handler =>
    handler.transformSource(_.switchMap(effects).share)
  }

  val merge: IO[Effect >--> EffectResult] = Handler.create[Effect].map { handler =>
    handler.transformSource(_.mergeMap(effects).share)
  }

  val concat: IO[Effect >--> EffectResult] = Handler.create[Effect].map { handler =>
    handler.transformSource(_.concatMap(effects).share)
  }


  def switch[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[EffectResult, A]): IO[E >--> A] =
    switch.map(_.collectPipe(f)(g))

  def merge[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[EffectResult, A]): IO[E >--> A] =
    merge.map(_.collectPipe(f)(g))

  def concat[E, A](f: PartialFunction[E, Effect])(g: PartialFunction[EffectResult, A]): IO[E >--> A] =
    concat.map(_.collectPipe(f)(g))


  def switch[A](g: PartialFunction[EffectResult, A]): IO[Effect >--> A] =
    switch.map(_.collectSource(g))

  def merge[A](g: PartialFunction[EffectResult, A]): IO[Effect >--> A] =
    merge.map(_.collectSource(g))

  def concat[E, A](g: PartialFunction[EffectResult, A]): IO[Effect >--> A] =
    concat.map(_.collectSource(g))
}


trait Effects[E, ER] extends EffectsOps {
  type Effect = E
  type EffectResult = ER
}
