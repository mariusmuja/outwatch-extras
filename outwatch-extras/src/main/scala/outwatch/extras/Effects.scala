package outwatch.extras

import outwatch.Sink
import outwatch.dom.Handlers
import rxscalajs.Observable

/**
  * Created by marius on 26/06/17.
  */
trait Effects {


  type Effect
  type EffectResult

  type EffectSink = Sink[Effect]
//  type EffectResultSource = Observable[EffectResult]

  private val handler = Handlers.createHandler[Effect]()

  val sink: EffectSink = handler

  val source: Observable[EffectResult] = handler.flatMap(effects).share

  def effects: Effect => Observable[EffectResult]
}
