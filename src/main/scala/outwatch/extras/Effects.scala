package outwatch.extras

import rxscalajs.Observable


object Effects {
  implicit class toFullHandler(handler: Effects.Handler) {
    @inline def full: Effects.HandlerFull = handler.applyOrElse(_, noEffects)
  }

  type Handler = PartialFunction[Action, Observable[Action]]
  val noEffects: Effects.HandlerFull = _ => Observable.empty
  val emptyHandler = PartialFunction.empty[Action, Observable[Action]]
  type HandlerFull = Action => Observable[Action]
}

trait Effects {
  import Effects.toFullHandler

  val effects: Effects.Handler = Effects.emptyHandler

  @inline def effectsFull: Effects.HandlerFull = effects.full

  protected def combineEffectsFirst(handlers: Effects.Handler*): Effects.Handler = handlers.reduce(_ orElse _)

  protected def combineEffects(handlers: Effects.Handler*): Effects.Handler = {
    case a: Action => Observable.from(handlers).flatMap(_.full(a))
  }
}
