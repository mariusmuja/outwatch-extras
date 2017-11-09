package outwatch.redux

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import outwatch.dom.{Handlers, Observable}

import scala.language.implicitConversions

/**
  * Created by marius on 11/06/17.
  */
object Store {

  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State
  ): IO[Action >--> State] = {

    Handlers.createHandler[Action](initActions: _*).map { handler =>
      val reducer: (State, Action) => State = (state, action) => state.evolve(action)

      val source: Observable[State] = handler
        .scan(initialState)(reducer)
        .startWith(Seq(initialState))

      HandlerPipe(handler, source.replay(1).refCount)
    }
  }


  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State,
    actionSource: Observable[Action],
  ): IO[Action >--> State] = {

    Handlers.createHandler[Action](initActions :_*).map { handler =>
      val reducer: (State, Action) => State = (state, action) => state.evolve(action)

      val source: Observable[State] = Observable.merge(handler, actionSource)
        .scan(initialState)(reducer)
        .startWith(Seq(initialState))

      HandlerPipe(handler, source.replay(1).refCount)
    }
  }


  def create[Action, Effect, State <: EvolvableEffectsState[Action, Effect, State]](
    initActions: Seq[Action],
    initialState: State,
    effectHandler: Effect >--> Action,
  ): IO[Action >--> State] = {

    Handlers.createHandler[Action](initActions :_*).map { handler =>

      val reducer: (State, Action) => State = (state, action) => {
        val (newState, effects) = state.evolve(action)
        effects.subscribe(effectHandler.sink.observer.onNext _)
        newState
      }

      val source: Observable[State] = Observable.merge(handler, effectHandler.source)
        .scan(initialState)(reducer)
        .startWith(Seq(initialState))

      HandlerPipe(handler, source.replay(1).refCount)
    }
  }
}
