package outwatch.extras.redux

import outwatch.dom.Observable

import scala.concurrent.Future
import scala.language.implicitConversions


/**
  * Created by marius on 23/05/17.
  */

trait EvolvableState[Action, State] {
  def evolve : Action => State
}

trait EvolvableStateWithEffects[Action, State, Effect] { self: State =>

  case class StateWithEffects(state: State, effects: Observable[Effect])

  def evolve : Action => StateWithEffects

  protected implicit def noEffect(state: State): StateWithEffects = StateWithEffects(state, Observable.empty)

  protected implicit def oneEffect(se : (State, Effect)): StateWithEffects = StateWithEffects(se._1, Observable.pure(se._2))

  protected implicit def oneFutureEffect(se : (State, Future[Effect])): StateWithEffects = StateWithEffects(se._1, Observable.fromFuture(se._2))

  protected implicit def fromTuple(se : (State, Observable[Effect])): StateWithEffects = StateWithEffects(se._1, se._2)

  protected implicit def justEffect(e : Effect): StateWithEffects = StateWithEffects(self, Observable.pure(e))

  protected implicit def justFutureEffect(e : Future[Effect]): StateWithEffects = StateWithEffects(self, Observable.fromFuture(e))

  protected implicit def justEffects(e : Observable[Effect]): StateWithEffects = StateWithEffects(self, e)

}


trait StatefulComponent {
  type Action

  protected type ComponentState = EvolvableState[Action, State]
  protected type State <: ComponentState
}

trait StatefulEffectsComponent {
  type Action
  type Effect

  protected type ComponentState = EvolvableStateWithEffects[Action, State, Effect]
  protected type State <: ComponentState
}