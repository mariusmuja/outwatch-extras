package outwatch.redux

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import monix.execution.{Ack, Cancelable}
import monix.reactive.Observable
import outwatch.dom.Handlers

import scala.concurrent.Future
import scala.language.implicitConversions

/**
  * Created by marius on 11/06/17.
  */
final case class Store[State, Action](source: Observable[State], sink: Sink[Action]) {

  def subscribe(f: State => Future[Ack]): Cancelable = source.subscribe(f)

  def share: Store[State, Action] = copy(source = source.share)

  def shareReplay(count: Int = 1): Store[State, Action] = copy(source = source.replay(1).refCount)
}

object Store {

  implicit def toSink[Action](store: Store[_, Action]): Sink[Action] = store.sink
  implicit def toSource[State](store: Store[State, _]): Observable[State] = store.source

  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State
  ): IO[Store[State, Action]] = {

    Handlers.createHandler[Action](initActions: _*).map { handler =>
      val reducer: (State, Action) => State = (state, action) => state.evolve(action)

      val source: Observable[State] = handler
        .scan(initialState)(reducer)
        .startWith(Seq(initialState))

      apply(source, handler).shareReplay()
    }
  }


  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State,
    actionSource: Observable[Action],
  ): IO[Store[State, Action]] = {

    Handlers.createHandler[Action](initActions :_*).map { handler =>
      val reducer: (State, Action) => State = (state, action) => state.evolve(action)

      val source: Observable[State] = Observable.merge(handler, actionSource)
        .scan(initialState)(reducer)
        .startWith(Seq(initialState))

      apply(source, handler).shareReplay()
    }
  }


  def create[Action, Effect, State <: EvolvableEffectsState[Action, Effect, State]](
    initActions: Seq[Action],
    initialState: State,
    effectHandler: Handler[Effect, Action],
  ): IO[Store[State, Action]] = {

    Handlers.createHandler[Action](initActions :_*).map { handler =>

      val reducer: (State, Action) => State = (state, action) => {
        val (newState, effects) = state.evolve(action)
        effects.subscribe(effectHandler.sink.observer.onNext _)
        newState
      }

      val source: Observable[State] = Observable.merge(handler, effectHandler.source)
        .scan(initialState)(reducer)
        .startWith(Seq(initialState))

      apply(source, handler).shareReplay()
    }
  }
}
