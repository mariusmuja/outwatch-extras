package outwatch.redux

import outwatch.Sink
import outwatch.dom.Observable

import scala.language.implicitConversions


/**
  * Created by marius on 23/05/17.
  */

trait EvolvableState[Action, State] {
  def evolve : Action => State
}

trait EvolvableStateWithEffects[Action, State, Effect] { self: State =>

  case class StateWithEffects(state: State, effects: Observable[Effect])

  protected implicit def noEffect(state: State): StateWithEffects = StateWithEffects(state, Observable.empty)

  protected implicit def oneEffect(se : (State, Effect)): StateWithEffects = StateWithEffects(se._1, Observable(se._2))

  protected implicit def fromTuple(se : (State, Observable[Effect])): StateWithEffects = StateWithEffects(se._1, se._2)

  protected implicit def justEffect(e : Effect): StateWithEffects = StateWithEffects(self, Observable(e))

  protected implicit def justEffects(e : Observable[Effect]): StateWithEffects = StateWithEffects(self, e)

  def evolve : Action => StateWithEffects
}


trait StatefulComponent {
  type Action
  type ActionSink = Sink[Action]

  protected type ComponentState = EvolvableState[Action, State]
  protected type State <: ComponentState
}

trait StatefulEffectsComponent {
  type Action
  type ActionSink = Sink[Action]

  type Effect
  type EffectSink = Sink[Effect]

  protected type ComponentState = EvolvableStateWithEffects[Action, State, Effect]
  protected type State <: ComponentState
}