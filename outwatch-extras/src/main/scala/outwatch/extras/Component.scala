package outwatch.extras

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
    val initState = init
    val handler = Handlers.createHandler[Action](initActions :_*)
    val source = handler
      .scan(initState)(reducer)
      .startWith(initState)
      .publishReplay(1)
      .refCount

    Store(source, handler)
  }

  protected def createStore(initActions: Seq[Action], actions: Observable[Action]): Store[State, Action] = {
    val initState = init
    val handler = Handlers.createHandler[Action](initActions :_*)
    val source = handler.merge(actions)
      .scan(initState)(reducer)
      .startWith(initState)
      .publishReplay(1)
      .refCount

    Store(source, handler)
  }
}
