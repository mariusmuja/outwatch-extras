package outwatch.extras.redux

/**
  * Created by marius on 23/05/17.
  */

@deprecated("Use Reducer typeclass", "")
trait StatefulComponent {
  type Action

  @deprecated("Use EvolvableState[Action, State]", "")
  protected type ComponentState = EvolvableState[Action, State]
  protected type State <: ComponentState
}

@deprecated("Use EffectsReducer typeclass", "")
trait StatefulEffectsComponent {
  type Action
  type Effect

  @deprecated("Use EvolvableStateWithEffects[Action, State, Effect]", "")
  protected type ComponentState = EvolvableStateWithEffects[Action, State, Effect]
  protected type State <: ComponentState
}
