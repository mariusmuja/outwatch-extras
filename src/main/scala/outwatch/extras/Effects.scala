package outwatch.extras

import rxscalajs.Observable


object Effects {
  type EffectHandler = PartialFunction[Action, Observable[Action]]
  type EffectHandlerFull = Action => Observable[Action]

  implicit class toFullEffectHandler(handler: EffectHandler) {
    private val noEffect: EffectHandlerFull = _ => Observable.empty

    def full: EffectHandlerFull = handler.applyOrElse(_, noEffect)
  }
}

trait Effects {
  import Effects._
  type EffectHandler = Effects.EffectHandler
  type EffectHandlerFull = Effects.EffectHandlerFull

  val effects: EffectHandler

  def effectsFull: EffectHandlerFull = effects.full

  protected def combineEffectsFirst(handlers: EffectHandler*): EffectHandler = handlers.reduce(_ orElse _)

  protected def combineEffects(handlers: EffectHandler*): EffectHandler = {
    case a: Action => Observable.from(handlers).flatMap(_.full(a))
  }
}
