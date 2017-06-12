package outwatch.extras

import outwatch.Sink
import outwatch.dom.Handlers.createHandler
import rxscalajs.Observable
import rxscalajs.subscription.Subscription
import scala.language.implicitConversions

/**
  * Created by marius on 11/06/17.
  */
final case class Store[State, Action](source: Observable[State], sink: Sink[Action]) {

  def subscribe(f: State => Unit): Subscription = source.subscribe(f)

  def map[S](project: State => S): Store[S, Action] = Store(source.distinct.map(project), sink)

  def share: Store[State, Action] = Store(source.share, sink)

  def shareReplay(count: Int = 1) = Store(source.publishReplay(1).refCount, sink)
}

object Store {

  implicit def toSink[Action](store: Store[_, Action]): Sink[Action] = store.sink
  implicit def toSource[State](store: Store[State, _]): Observable[State] = store.source

  def apply[State, Action](initialState: State, reducer: (State, Action) => State): Store[State, Action] = {
    val sink: Observable[Action] with Sink[Action] = createHandler[Action]()
    val source: Observable[State] = sink
      .scan(initialState)(reducer)
      .startWith(initialState)

    apply(source, sink)
  }
}