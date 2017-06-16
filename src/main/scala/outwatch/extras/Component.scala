package outwatch.extras


/**
  * Created by marius on 23/05/17.
  */


object Component {
  type Reducer[State] = PartialFunction[(State, Action), State]
  type ReducerFull[State] = (State, Action) =>  State
}


trait PureComponent {

  trait EvolvableState_[State] {
    type StateEvolver =  PartialFunction[Action, State]
    def evolve : StateEvolver
  }

  type EvolvableState = EvolvableState_[State]
  type State <: EvolvableState

  val reducer: Reducer = {
    case (state, action) if state.evolve.isDefinedAt(action) => state.evolve(action)
  }

  type Reducer = Component.Reducer[State]
  type ReducerFull = Component.ReducerFull[State]

  @inline def reducerFull: ReducerFull = reducer.full

  private implicit class toFullReducer[S](reducer: Component.Reducer[S]) {
    private val ignoreActionReducer: Component.ReducerFull[S] = (s, _) => s
    @inline def full: Component.ReducerFull[S] = (s,a) => reducer.applyOrElse((s,a), ignoreActionReducer.tupled)
  }
}

trait Component extends PureComponent with Effects