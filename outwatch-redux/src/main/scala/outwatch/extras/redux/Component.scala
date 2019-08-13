package outwatch.extras.redux

/**
  * Created by marius on 23/05/17.
  */

@deprecated("Use StateReducer typeclass", "")
trait EvolvableState[Action, State] { self: State =>
  def evolve: Action => State
}


@deprecated("Use StateEffectsReducer typeclass", "")
trait EvolvableStateWithEffects[Action, State, Effect] {
  self: State =>

  def evolve: Action => StateWithEffects[State, Effect]

}


@deprecated("Use StateReducer typeclass", "")
trait StatefulComponent {
  type Action

  protected type ComponentState = EvolvableState[Action, State]
  protected type State <: ComponentState
}

@deprecated("Use StateEffectsReducer typeclass", "")
trait StatefulEffectsComponent {
  type Action
  type Effect

  protected type ComponentState = EvolvableStateWithEffects[Action, State, Effect]
  protected type State <: ComponentState
}
