package outwatch.redux

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import outwatch.dom.{Handler, Pipe, Observable}
import outwatch.extras.>-->

/**
  * Created by marius on 11/06/17.
  */
object Store {

  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions: _*).map { handler =>
      val reducer: (State, Action) => State = (state, action) => state.evolve(action)

      val source: Observable[State] = handler
        .scan(initialState)(reducer)
        .startWith(Seq(initialState))

      Pipe(handler, source.replay(1).refCount)
    }
  }


  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State,
    actionSource: Observable[Action],
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions :_*).map { handler =>
      val reducer: (State, Action) => State = (state, action) => state.evolve(action)

      val source: Observable[State] = Observable.merge(handler, actionSource)
        .scan(initialState)(reducer)
        .startWith(Seq(initialState))

      Pipe(handler, source.replay(1).refCount)
    }
  }


  def create[Action, Effect, State <: EvolvableStateWithEffects[Action, State, Effect]](
    initActions: Seq[Action],
    initialState: State,
    effects: IO[Effect >--> Action],
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions :_*).flatMap { handler =>
      effects.map { effectHandler =>

        val reducer: (State, Action) => State = (state, action) => {
//          println(s" --> In reducer: $action")
          val se = state.evolve(action)
          se.effects.subscribe(effectHandler.observer.onNext _)
          se.state
        }

        val source: Observable[State] = Observable.merge(handler, effectHandler)
          .scan(initialState)(reducer)
          .startWith(Seq(initialState))

        Pipe(handler, source.replay(1).refCount)
      }
    }
  }

  def create[Action, Effect, State <: EvolvableStateWithEffects[Action, State, Effect]](
    initActions: Seq[Action],
    initialState: State,
    effects1: IO[Effect >--> Action],
    effects2: IO[Effect >--> Action],
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions :_*).flatMap { handler =>
      effects1.flatMap { effectHandler1 =>
        effects2.map { effectHandler2 =>

          val reducer: (State, Action) => State = (state, action) => {
            //          println(s" --> In reducer: $action")
            val se = state.evolve(action)
            se.effects.subscribe(effectHandler1.observer.onNext _)
            se.effects.subscribe(effectHandler2.observer.onNext _)
            se.state
          }

          val source: Observable[State] = Observable.merge(handler, effectHandler1, effectHandler2)
            .scan(initialState)(reducer)
            .startWith(Seq(initialState))

          Pipe(handler, source.replay(1).refCount)
        }
      }
    }
  }
}
