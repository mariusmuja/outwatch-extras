package outwatch.extras

import rxscalajs.Observable
import rxscalajs.subscription.AnonymousSubscription

trait ComponentWithEffects {

  protected trait StateLike[State] {
    def evolve : PartialFunction[Action, State]

    def effects : EffectsHandlerNoState
  }

  type ComponentState = StateLike[State]
  type State <: ComponentState

  type Reducer = PartialFunction[(State, Action), State]
  type ReducerFull = (State, Action) =>  State

  type EffectsHandlerNoState = PartialFunction[Action, Observable[Action]]
  type EffectsHandlerNoStateFull = Action => Observable[Action]

  type EffectsHandler = PartialFunction[(State, Action), Observable[Action]]
  type EffectsHandlerFull = (State, Action) => Observable[Action]

  def reducer: Reducer = {
    case (state, action) if state.evolve.isDefinedAt(action) => state.evolve(action)
  }

  def effects : EffectsHandlerNoState = PartialFunction.empty

  def effectsInState : EffectsHandler = {
    case (state, action) if state.effects.isDefinedAt(action) => state.effects(action)
  }

  private var subStateEffects : Option[AnonymousSubscription] = None
  private var subEffects : Option[AnonymousSubscription] = None

  def store(handler: Handler[Action], initState: State, initAction: Action): Store[State, Action] = {

    val initWithEffects = (initState, Observable.just[Action]())
    val source = handler.startWith(initAction)
      .scan(initWithEffects) { case ((s, _), a) =>
        (reducer.full(s, a), effectsInState.full(s, a))
      }
      .publishReplay(1)
      .refCount

    subStateEffects.foreach(_.unsubscribe())
    subStateEffects = Option(handler <-- source.flatMap(_._2))
    subEffects.foreach(_.unsubscribe())
    subEffects = Option(handler <-- handler.flatMap(effects.applyOrElse(_, (_: Action) => Observable.empty)))

    Store(source.map(_._1), handler)
  }

  private implicit class toFullReducer(reducer: Reducer) {
    private val ignoreActionReducer: ReducerFull = (s, _) => s
    @inline def full: ReducerFull = (s,a) => reducer.applyOrElse((s,a), ignoreActionReducer.tupled)
  }


  private implicit class toFullHandler(handler: EffectsHandler) {
    private val noEffects: EffectsHandlerFull = (_,_) => Observable.empty
    @inline def full: EffectsHandlerFull = (s,a) => handler.applyOrElse((s,a), noEffects.tupled)
  }
}