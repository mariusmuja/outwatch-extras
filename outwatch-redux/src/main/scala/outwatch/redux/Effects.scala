package outwatch.redux

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import outwatch.dom.{Handlers, Observable}

import scala.language.implicitConversions

trait Effects[Effect, EffectResult] {

  def effects: Effect => Observable[EffectResult]

  val switch: IO[Effect >--> EffectResult] = Handlers.createHandler[Effect].map { handler =>
    HandlerPipe(handler, handler.switchMap(effects).share)
  }

  val merge: IO[Effect >--> EffectResult] = Handlers.createHandler[Effect].map { handler =>
    HandlerPipe(handler, handler.mergeMap(effects).share)
  }

  val concat: IO[Effect >--> EffectResult] = Handlers.createHandler[Effect].map { handler =>
    HandlerPipe(handler, handler.concatMap(effects).share)
  }
}