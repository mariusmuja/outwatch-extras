package outwatch.redux

import outwatch.Sink
import outwatch.dom.Handlers
import rxscalajs.Observable


/**
  * Created by marius on 23/05/17.
  */

trait Component {

  type Action
  type ActionSink = Sink[Action]

  protected trait StateLike[State] {
    def evolve : Action => State
  }

  protected type ComponentState = StateLike[State]
  protected type State <: ComponentState

  private val reducer: (State, Action) =>  State = (state, action) => state.evolve(action)

  def init: State

  protected def createStore(initActions: Seq[Action]): Store[State, Action] = {
    val handler = Handlers.createHandler[Action](initActions :_*)
    Store(handler, init, reducer).shareReplay()
  }

  protected def createStore(initActions: Seq[Action], actions: Observable[Action]): Store[State, Action] = {
    val handler = Handlers.createHandler[Action](initActions :_*)
    Store(handler, init, actions, reducer).shareReplay()
  }
}
