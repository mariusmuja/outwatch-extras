package outwatch.redux

import outwatch.dom.Observable

import scala.language.implicitConversions


/**
  * Created by marius on 23/05/17.
  */

trait EvolvableState[Action, State] {
  def evolve : Action => State
}

trait EvolvableEffectsState[Action, Effect, State] { self: State =>

  protected implicit def noEffect(state: State): (State, Observable[Effect]) = (state, Observable.empty)

  protected implicit def oneEffect(se : (State, Effect)): (State, Observable[Effect]) = (se._1, Observable(se._2))

  protected implicit def justEffect(e : Effect): (State, Observable[Effect]) = (self, Observable(e))

  def evolve : Action => (State, Observable[Effect])
}


trait StatefulComponent {
  type Action
  type ActionSink = Sink[Action]

  protected type ComponentState = EvolvableState[Action, State]
  protected type State <: ComponentState

  type >-->[-I, +O] = outwatch.redux.Pipe[I, O]
  val >--> = outwatch.redux.Pipe
}

trait StatefulEffectsComponent {
  type Action
  type ActionSink = Sink[Action]

  type Effect
  type EffectSink = Sink[Effect]

  protected type ComponentState = EvolvableEffectsState[Action, Effect, State]
  protected type State <: ComponentState

  type >-->[-I, +O] = outwatch.redux.Pipe[I, O]
  val >--> = outwatch.redux.Pipe
}