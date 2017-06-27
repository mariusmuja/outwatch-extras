package outwatch.extras

import outwatch.Sink
import outwatch.dom.Handlers
import rxscalajs.Observable

/**
  * Created by marius on 26/06/17.
  */
trait Effects {

  type EffectSink = Sink[Effect]

  type Effect

  type EffectResult

  private val handler = Handlers.createHandler[Effect]()

  val sink: EffectSink = handler

  val source: Observable[EffectResult] = handler.flatMap(effects)

  def effects: Effect => Observable[EffectResult]
}
