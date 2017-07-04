package outwatch.redux

import outwatch.Sink
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


  def apply[State, Action](
    handler: Handler[Action],
    initialState: State,
    reducer: (State, Action) => State
  ): Store[State, Action] = {

    val source: Observable[State] = handler
      .scan(initialState)(reducer)
      .startWith(initialState)

    apply(source, handler)
  }

  def apply[State, Action](
    handler: Handler[Action],
    initialState: State,
    actionSource: Observable[Action],
    reducer: (State, Action) => State
  ): Store[State, Action] = {

    val source: Observable[State] = handler.merge(actionSource)
      .scan(initialState)(reducer)
      .startWith(initialState)

    apply(source, handler)
  }

}