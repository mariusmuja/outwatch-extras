package outwatch.redux

import outwatch.dom.Handlers
import rxscalajs.Observable

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

trait Effects[Effect, EffectResult] {

  private val handler = Handlers.createHandler[Effect]()

  val sink: Sink[Effect] = handler

  // convenience implicit conversions
  protected implicit def fromFutureResult(
    result: Future[EffectResult]
  )(implicit ex: ExecutionContext): Observable[EffectResult] = Observable.from(result)

  //    protected implicit def fromResult(result: EffectResult): Observable[EffectResult] = Observable(result)

  def effects: Effect => Observable[EffectResult]

  object Switch extends Handler[Effect, EffectResult] {
    def sink: Sink[Effect] = Effects.this.sink
    lazy val source: Source[EffectResult] = handler.switchMap(effects).share
  }

  object Merge extends Handler[Effect, EffectResult] {
    def sink: Sink[Effect] = Effects.this.sink
    lazy val source: Source[EffectResult] = handler.mergeMap(effects).share
  }

  object Concat extends Handler[Effect, EffectResult] {
    def sink: Sink[Effect] = Effects.this.sink
    lazy val source: Source[EffectResult] = handler.concatMap(effects).share
  }

}