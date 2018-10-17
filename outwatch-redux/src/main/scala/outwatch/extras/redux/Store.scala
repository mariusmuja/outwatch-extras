package outwatch.extras.redux

import cats.effect.IO
import monix.execution.Cancelable.IsDummy
import monix.execution.Scheduler.Implicits.global
import monix.execution.cancelables.CompositeCancelable
import monix.execution.misc.NonFatal
import org.scalajs.dom
import outwatch.dom.{Handler, Observable}
import outwatch.extras.>-->

/**
  * Created by marius on 11/06/17.
  */
object Store {


  private def stateReducer[Action, State <: EvolvableState[Action, State]]: (State, Action) => State =
    (state, action) => try {
      state.evolve(action)
    } catch {
      case NonFatal(e) =>
        dom.console.error(e.getMessage)
        state
    }


  def create[Action, State <: EvolvableState[Action, State]](
    initActions: Seq[Action],
    initialState: State
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions: _*).map { handler =>
      handler.transformSource { handler =>
        val reducer = stateReducer[Action, State]
        handler
          .scan(initialState)(reducer)
          .behavior(initialState).refCount
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
        val reducer = stateReducer[Action, State]
        Observable.merge(handler, actionSource)
          .scan(initialState)(reducer)
          .behavior(initialState).refCount
      }
    }
  }

  def create[Action, Effect, State <: EvolvableStateWithEffects[Action, State, Effect]](
    initActions: Seq[Action],
    initialState: State,
    effects: IO[Effect >--> Action]
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions: _*).flatMap { handler =>
      effects.map { effectHandler =>
        handler.transformSource { handler =>
          val sub = CompositeCancelable()

          val reducer: (State, Action) => State = (state, action) => try {
            val se = state.evolve(action)
            val cancelable = se.effects.subscribe(
              e => effectHandler.observer.feed(e :: Nil),
              e => dom.console.error(e.getMessage) // just log the error, don't push it into the handler's observable, because it would stop the scan "loop"
            )
            if (!cancelable.isInstanceOf[IsDummy]) sub += cancelable
            se.state
          } catch {
            case NonFatal(e) =>
              dom.console.error(e.getMessage)
              state
          }

          Observable.merge(handler, effectHandler)
            .scan(initialState)(reducer)
            .behavior(initialState).refCount
            .doOnSubscriptionCancel(() => sub.cancel())
        }
      }
    }
  }

  def create[Action, Effect, State <: EvolvableStateWithEffects[Action, State, Effect]](
    initActions: Seq[Action],
    initialState: State,
    actionSource: Observable[Action],
    effects: IO[Effect >--> Action]
  ): IO[Action >--> State] = {

    Handler.create[Action](initActions: _*).flatMap { handler =>
      effects.map { effectHandler =>
        handler.transformSource { handler =>
          val sub = CompositeCancelable()

          val reducer: (State, Action) => State = (state, action) => try {
            val se = state.evolve(action)
            val cancelable = se.effects.subscribe(
              e => effectHandler.observer.feed(e :: Nil),
              e => dom.console.error(e.getMessage)
            )
            if (!cancelable.isInstanceOf[IsDummy]) sub += cancelable
            se.state
          } catch {
            case NonFatal(e) =>
              dom.console.error(e.getMessage)
              state
          }

          Observable.merge(handler, actionSource, effectHandler)
            .scan(initialState)(reducer)
            .behavior(initialState).refCount
            .doOnSubscriptionCancel(() => sub.cancel())
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
          val sub = CompositeCancelable()

          val reducer: (State, Action) => State = (state, action) => try {
            val se = state.evolve(action)
            effectHandlers.foreach { effectHandler =>
              val cancelable = se.effects.subscribe(
                e => effectHandler.observer.feed(e :: Nil),
                e => dom.console.error(e.getMessage)
              )
              if (!cancelable.isInstanceOf[IsDummy]) sub += cancelable
            }
            se.state
          } catch {
            case NonFatal(e) =>
              dom.console.error(e.getMessage)
              state
          }

          Observable.merge(actionSource :: effectHandlers: _*)
            .scan(initialState)(reducer)
            .behavior(initialState).refCount
            .doOnSubscriptionCancel(() => sub.cancel())
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
