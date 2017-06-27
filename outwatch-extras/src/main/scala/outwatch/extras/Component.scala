package outwatch.extras

import outwatch.Sink
import outwatch.dom.Handlers


/**
  * Created by marius on 23/05/17.
  */

trait Component {

  protected trait Action

  type ActionSink = Sink[Action]

  private object Action {
    object Nop extends Action
  }

  protected trait StateLike[State] {
    def evolve : PartialFunction[Action, State]
  }

  protected type ComponentState = StateLike[State]
  protected type State <: ComponentState

  private type Reducer = (State, Action) =>  State

  private val reducer: Reducer = {
    case (state, action) if state.evolve.isDefinedAt(action) => state.evolve(action)
    case (state, _) => state
  }

  def init: State

  protected def createStore(initActions: Seq[Action]): Store[State, Action] = {
    val initActionsOrNop = if (initActions.isEmpty) Seq(Action.Nop) else initActions
    val handler = Handlers.createHandler[Action](initActionsOrNop :_*)
    val source = handler
      .scan(init)(reducer)
      .publishReplay(1)
      .refCount

    Store(source, handler)
  }
}
