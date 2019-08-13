package outwatch.extras.redux

import monix.eval.TaskLike
import monix.execution.Cancelable
import monix.execution.cancelables.CompositeCancelable
import org.scalajs.dom
import outwatch.Handler
import outwatch.dom.{IO, Observable}
import outwatch.extras.>-->

import scala.language.{higherKinds, implicitConversions}
import scala.util.control.NonFatal

/**
  * Created by marius on 11/06/17.
  */



trait StateReducer[Action, State] {
  def reduce(self: State, action: Action): State
}

object StateReducer {

  implicit def funcReducer[A, S](reducer: (S, A) => S): StateReducer[A, S] = (state: S, action: A) => reducer(state, action)

  @deprecated("", "")
  implicit def evolvableStateReducer[A, S <: EvolvableState[A, S]]: StateReducer[A, S] = (state: S, action: A) => state.evolve(action)
}


case class StateWithEffects[State, Effect](state: State, effects: Observable[Effect]) {
  def tupled: (State, Observable[Effect]) = (state, effects)
}



object StateWithEffects {
  implicit def noEffect[S, E](state: S): StateWithEffects[S, E] = StateWithEffects(state, Observable.empty)

  implicit def oneEffect[S, E](se : (S, E)): StateWithEffects[S, E] = StateWithEffects(se._1, Observable.pure(se._2))

  implicit def oneFutureEffect[F[_]: TaskLike, S, E](se : (S, F[E])): StateWithEffects[S, E] = StateWithEffects(se._1, Observable.fromTaskLike(se._2))

  implicit def fromTuple[S, E](se : (S, Observable[E])): StateWithEffects[S, E] = StateWithEffects(se._1, se._2)
}


trait StateEffectsReducer[Action, State, Effect] {
  def reduce(self: State, action: Action): (State, Observable[Effect])
}

object StateEffectsReducer {
  implicit def funcReducer[A, S, E](reducer: (S, A) => (S, Observable[E])): StateEffectsReducer[A, S, E] = (state: S, action: A) => reducer(state, action)

  @deprecated("", "")
  implicit def evolvableStateReducer[A, E, S <: EvolvableStateWithEffects[A, S, E]]: StateEffectsReducer[A, S, E] = (state: S, action: A) => state.evolve(action).tupled
}


object Store {

  def create[Action, State](
    initialState: State
  )(implicit r: StateReducer[Action, State]): IO[Action >--> State] = {
    create(Seq.empty, initialState)(r)
  }

  def create[Action, State](
    initActions: Seq[Action],
    initialState: State,
    actionSource: Observable[Action] = Observable.empty,
  )(implicit r: StateReducer[Action, State]): IO[Action >--> State] = {

    IO.deferAction { implicit scheduler =>
      Handler.create[Action](initActions: _*).map { handler =>
        handler.transformSource { handler =>
          val reducer: (State, Action) => State = (state, action) => try {
            r.reduce(state, action)
          } catch {
            case NonFatal(e) =>
              dom.console.error(e.getMessage)
              state
          }
          val actions = if (actionSource == Observable.empty) {
            handler
          } else {
            Observable(handler, actionSource).merge
          }

          actions.scan(initialState)(reducer)
            .behavior(initialState).refCount
        }
      }
    }
  }



  def create[Action, Effect, State](
    initActions: Seq[Action],
    initialState: State,
    effects: IO[Effect >--> Action],
    actionSource: Observable[Action] = Observable.empty
  )(implicit r: StateEffectsReducer[Action, State, Effect]): IO[Action >--> State] = {

    IO.deferAction { implicit scheduler =>
      Handler.create[Action](initActions: _*).flatMap { handler =>
        effects.map { effectHandler =>
          handler.transformSource { handler =>
            val sub = CompositeCancelable()

            val reducer: (State, Action) => State = (state, action) => try {
              val (newState, effects) = r.reduce(state, action)
              val cancelable = effects.subscribe(
                e => effectHandler.feed(e :: Nil),
                e => dom.console.error(e.getMessage)
              )
              if (cancelable != Cancelable.empty) sub += cancelable
              newState
            } catch {
              case NonFatal(e) =>
                dom.console.error(e.getMessage)
                state
            }

            val actions = if (actionSource == Observable.empty) {
              Observable(handler, effectHandler).merge
            } else {
              Observable(handler, actionSource, effectHandler).merge
            }
            actions.scan(initialState)(reducer)
              .behavior(initialState).refCount
              .doOnSubscriptionCancelF(() => sub.cancel())
          }
        }
      }
    }
  }

  def create[Action, Effect, State](
    initActions: Seq[Action],
    initialState: State,
    effects: Seq[IO[Effect >--> Action]]
  )(implicit r: StateEffectsReducer[Action, State, Effect]): IO[Action >--> State] = {

    import cats.instances.list._
    import cats.syntax.traverse._

    IO.deferAction { implicit scheduler =>
      Handler.create[Action](initActions: _*).flatMap { actions =>
        effects.toList.sequence.map { effectHandlers =>
          actions.transformSource { actionSource =>
            val sub = CompositeCancelable()

            val reducer: (State, Action) => State = (state, action) => try {
              val (newState, effects) = r.reduce(state, action)
              effectHandlers.foreach { effectHandler =>
                val cancelable = effects.subscribe(
                  e => effectHandler.feed(e :: Nil),
                  e => dom.console.error(e.getMessage)
                )
                if (cancelable != Cancelable.empty) sub += cancelable
              }
              newState
            } catch {
              case NonFatal(e) =>
                dom.console.error(e.getMessage)
                state
            }

            Observable(actionSource :: effectHandlers: _*).merge
              .scan(initialState)(reducer)
              .behavior(initialState).refCount
              .doOnSubscriptionCancelF(() => sub.cancel())
          }
        }
      }
    }
  }

  def create[Action, Effect, State](
    initActions: Seq[Action],
    initialState: State,
    effects1: IO[Effect >--> Action],
    effects2: IO[Effect >--> Action],
    effects: IO[Effect >--> Action]*
  )(implicit r: StateEffectsReducer[Action, State, Effect]): IO[Action >--> State] = create(initActions, initialState, effects1 :: effects2 :: effects.toList)

}
