package outwatch.extras

import rxscalajs.Observable


object Effects {
  type Handler[State] = PartialFunction[(State, Action), Observable[Action]]
  type HandlerFull[State] = (State, Action) => Observable[Action]

  def noEffects[State]: HandlerFull[State] = (_, _) => Observable.empty
}

trait Effects { self : Component =>
  type EffectsHandler = Effects.Handler[State]
  type EffectsHandlerFull = Effects.HandlerFull[State]


  def effects: EffectsHandler

  private implicit class toFullHandler(handler: EffectsHandler) {
    val noEffects: EffectsHandlerFull = (_,_) => Observable.empty
    @inline def full: EffectsHandlerFull = (s,a) => handler.applyOrElse((s,a), noEffects.tupled)
  }

  @inline def effectsFull: EffectsHandlerFull = effects.full

  protected def combineEffectsFirst(handlers: EffectsHandler*): EffectsHandler = handlers.reduce(_ orElse _)

  protected def combineEffects(handlers: EffectsHandler*): EffectsHandler = {
    case (state, act) => Observable.from(handlers).flatMap(handler => handler.full(state, act))
  }

  protected def subEffectHandler[S, SS](handler: Effects.Handler[SS], zoom: S => SS): Effects.Handler[S] = {
    case (s, a) => handler.applyOrElse((zoom(s), a), (_: (SS, Action)) => Observable.empty)
  }
}

trait NoEffects { self : Effects =>
  override val effects: EffectsHandler = PartialFunction.empty
}