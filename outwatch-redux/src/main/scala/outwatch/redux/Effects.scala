package outwatch.redux

import monix.execution.Scheduler.Implicits.global
import outwatch.dom.{Handlers, Observable}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

trait Effects[Effect, EffectResult] {

  private val handler = Handlers.createHandler[Effect]().unsafeRunSync()

  val sink: Sink[Effect] = handler

  // convenience implicit conversions
  protected implicit def fromFutureResult(
    result: Future[EffectResult]
  )(implicit ex: ExecutionContext): Observable[EffectResult] = Observable.fromFuture(result)

  //    protected implicit def fromResult(result: EffectResult): Observable[EffectResult] = Observable(result)

  def effects: Effect => Observable[EffectResult]

  object Switch extends >-->[Effect, EffectResult] {
    def sink: Sink[Effect] = handler
    lazy val source: Source[EffectResult] = handler.switchMap(effects).share
  }

  object Merge extends >-->[Effect, EffectResult] {
    def sink: Sink[Effect] = handler
    lazy val source: Source[EffectResult] = handler.mergeMap(effects).share
  }

  object Concat extends >-->[Effect, EffectResult] {
    def sink: Sink[Effect] = handler
    lazy val source: Source[EffectResult] = handler.concatMap(effects).share
  }

}