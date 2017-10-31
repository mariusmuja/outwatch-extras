package outwatch.redux

import outwatch.dom.Handlers
import rxscalajs.Observable
import rxscalajs.subscription.Subscription

import scala.language.implicitConversions

/**
  * Created by marius on 11/06/17.
  */
final case class Store[State, Action](source: Observable[State], sink: Sink[Action]) {

  def subscribe(f: State => Unit): Subscription = source.subscribe(f)

  def share: Store[State, Action] = Store(source.share, sink)

  def shareReplay(count: Int = 1) = Store(source.publishReplay(1).refCount, sink)
}

object Store {

  implicit def toSink[Action](store: Store[_, Action]): Sink[Action] = store.sink
  implicit def toSource[State](store: Store[State, _]): Observable[State] = store.source

  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State
  ): Store[State, Action] = {

    val handler = Handlers.createHandler[Action](initActions: _*)
    val reducer: (State, Action) => State = (state, action) => state.evolve(action)

    val source: Observable[State] = handler
      .scan(initialState)(reducer)
      .startWith(initialState)

    apply(source, handler).shareReplay()
  }

  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State,
    actionSource: Observable[Action],
  ): Store[State, Action] = {

    val handler = Handlers.createHandler[Action](initActions: _*)
    val reducer: (State, Action) => State = (state, action) => state.evolve(action)

    val source: Observable[State] = handler.merge(actionSource)
      .scan(initialState)(reducer)
      .startWith(initialState)

    apply(source, handler).shareReplay()
  }


  def create[Action, Effect, State <: EvolvableEffectsState[Action, Effect, State]](
    initActions: Seq[Action],
    initialState: State,
    effectHandler: Handler[Effect, Action],
  ): Store[State, Action] = {

    val handler = Handlers.createHandler[Action](initActions: _*)

    val reducer: (State, Action) => State = (state, action) => {
      val (newState, effects) = state.evolve(action)
      effects.subscribe(effectHandler.sink.observer.next _)
      newState
    }

    val source: Observable[State] = handler.merge(effectHandler.source)
      .scan(initialState)(reducer)
      .startWith(initialState)

    apply(source, handler).shareReplay()
  }
}