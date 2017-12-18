package outwatch.extras.redux

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import outwatch.dom.{Handler, Observable}
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

      handler.transformSource { handler =>
        val reducer: (State, Action) => State = (state, action) => state.evolve(action)

        handler
          .scan(initialState)(reducer)
          .startWith(Seq(initialState))
          .replay(1).refCount

      }
    }
  }


  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State,
    actionSource: Observable[Action]
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions :_*).map { handler =>
      handler.transformSource { handler =>

        val reducer: (State, Action) => State = (state, action) => state.evolve(action)

        Observable.merge(handler, actionSource)
          .scan(initialState)(reducer)
          .startWith(Seq(initialState))
          .replay(1).refCount
      }
    }
  }


  def create[Action, Effect, State <: EvolvableStateWithEffects[Action, State, Effect]](
    initActions: Seq[Action],
    initialState: State,
    effects: IO[Effect >--> Action]
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions :_*).flatMap { handler =>
      effects.map { effectHandler =>

        handler.transformSource { handler =>

          val reducer: (State, Action) => State = (state, action) => {
            //          println(s" --> In reducer: $action")
            val se = state.evolve(action)
            se.effects.subscribe(effectHandler.observer.onNext _)
            se.state
          }

          Observable.merge(handler, effectHandler)
            .scan(initialState)(reducer)
            .startWith(Seq(initialState))
            .replay(1).refCount
        }
      }
    }
  }

  def create[Action, Effect, State <: EvolvableStateWithEffects[Action, State, Effect]](
    initActions: Seq[Action],
    initialState: State,
    effects: Seq[IO[Effect >--> Action]]
  ): IO[Action >--> State] = {

    import cats.instances.list._
    import cats.syntax.traverse._

    Handler.create[Action](initActions: _*).flatMap { actions =>
      // type annotation to keep IDEA typechecker happy
      (effects.toList.sequence: IO[List[Effect >--> Action]]).map { effectHandlers =>

        actions.transformSource { actionSource =>

          val reducer: (State, Action) => State = (state, action) => {
            //          println(s" --> In reducer: $action")
            val se = state.evolve(action)
            effectHandlers.foreach(effectHandler =>
              se.effects.subscribe(effectHandler.observer.onNext _)
            )
            se.state
          }

          Observable.merge(actionSource :: effectHandlers: _*)
            .scan(initialState)(reducer)
            .startWith(Seq(initialState))
            .replay(1).refCount
        }
      }
    }
  }

  def create[Action, Effect, State <: EvolvableStateWithEffects[Action, State, Effect]](
    initActions: Seq[Action],
    initialState: State,
    effects1: IO[Effect >--> Action],
    effects2: IO[Effect >--> Action],
    effects: IO[Effect >--> Action]*
  ): IO[Action >--> State] = create(initActions, initialState, effects1 :: effects2 :: effects.toList)

}
