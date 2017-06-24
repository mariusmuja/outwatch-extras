package outwatch.extras

import outwatch.dom.VNode
import outwatch.styles.ComponentStyle
import rxscalajs.Observable
import rxscalajs.subscription.AnonymousSubscription


/**
  * Created by marius on 23/05/17.
  */

trait ComponentBase {

  protected trait StateLike[State] {
    def evolve : PartialFunction[Action, State]
  }

  type ComponentState = StateLike[State]
  type State <: ComponentState

  type Reducer = (State, Action) =>  State

  private val reducer: Reducer = {
    case (state, action) if state.evolve.isDefinedAt(action) => state.evolve(action)
    case (state, _) => state
  }

  def init: State

  private[extras] def store(handler: Handler[Action], initState: State, initActions: Action*): Store[State, Action] = {

    val initActionsOrNop = if (initActions.isEmpty) Seq(Action.Nop) else initActions
    val source = handler.source.startWithMany(initActionsOrNop : _*)
      .scan(initState)(reducer)
      .publishReplay(1)
      .refCount

    Store(source, handler)
  }
}


trait ComponentWithEffectsBase {

  protected trait StateLike[State] {
    def evolve : PartialFunction[Action, State]

    def effects : PartialFunction[Action, Observable[Action]]
  }

  type ComponentState = StateLike[State]
  type State <: ComponentState

  type Reducer = (State, Action) =>  State
  type EffectsHandler = (State, Action) => Observable[Action]

  private val reducer: Reducer = {
    case (state, action) if state.evolve.isDefinedAt(action) => state.evolve(action)
    case (state, _) => state
  }

  private val effects : EffectsHandler = {
    case (state, action) if state.effects.isDefinedAt(action) => state.effects(action)
    case (_, _) => Observable.empty
  }

  def init: State

  private var effectsSubscription : Option[AnonymousSubscription] = None

  private[extras] def store(handler: Handler[Action], initState: State,  initActions: Action*): Store[State, Action] = {

    val initWithEffects = (initState, Observable.just[Action]())
    val initActionsOrNop = if (initActions.isEmpty) Seq(Action.Nop) else initActions
    val source = handler.source.startWithMany(initActionsOrNop : _*)
      .scan(initWithEffects) { case ((s, _), a) =>
        (reducer(s, a), effects(s, a))
      }
      .publishReplay(1)
      .refCount

    org.scalajs.dom.console.log(""+effectsSubscription)
    effectsSubscription.foreach(_.unsubscribe())
    effectsSubscription = Some(handler.sink <-- source.flatMap(_._2))

//    val sinkWithEffects = handler.sink.redirect[Action](sinkObs => sinkObs.merge(source.flatMap(_._2)))

    Store(source.map(_._1), handler)
  }
}


trait Component extends ComponentBase {

  def view(handler: Store[State, Action]): VNode

  def apply(handler: Handler[Action], initActions: Action*): VNode = {
    view(store(handler, init, initActions : _*))
  }
}


trait StyledComponent extends ComponentBase with ComponentStyle {

  def view(handler: Store[State, Action])(implicit stl: Style): VNode

  def apply(handler: Handler[Action], initActions: Action*)(implicit stl: Style): VNode = {
    view(store(handler, init, initActions : _*))
  }
}


trait ComponentWithEffects extends ComponentWithEffectsBase {
  def view(handler: Store[State, Action]): VNode

  def apply(handler: Handler[Action], initActions: Action*): VNode = {
    view(store(handler, init, initActions : _*))
  }
}


trait StyledComponentWithEffects extends ComponentWithEffectsBase with ComponentStyle {

  def view(handler: Store[State, Action])(implicit stl: Style): VNode

  def apply(handler: Handler[Action], initActions: Action*)(implicit stl: Style): VNode = {
    view(store(handler, init, initActions : _*))
  }
}