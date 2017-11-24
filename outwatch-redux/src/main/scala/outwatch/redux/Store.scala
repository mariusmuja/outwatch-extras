package outwatch.redux

import cats.effect.IO
import outwatch.extras.>-->
import outwatch.{Handler, Pipe}
import rxscalajs.Observable

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
        .startWith(initialState)

      Pipe(handler, source.publishReplay(1).refCount)
    }
  }


  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State,
    actionSource: Observable[Action],
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions :_*).map { handler =>
      val reducer: (State, Action) => State = (state, action) => state.evolve(action)

      val source: Observable[State] = handler.merge(actionSource)
        .scan(initialState)(reducer)
        .startWith(initialState)

      Pipe(handler, source.publishReplay(1).refCount)
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
          val se = state.evolve(action)
          se.effects.subscribe((e: Effect) => effectHandler.observer.next(e))
          se.state
        }

      val source: Observable[State] = effectHandler.merge(handler)
        .scan(initialState)(reducer)
        .startWith(initialState)

        Pipe(handler, source.publishReplay(1).refCount)
      }
    }
  }
}
