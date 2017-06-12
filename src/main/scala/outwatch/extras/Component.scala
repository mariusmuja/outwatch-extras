package outwatch.extras

import com.softwaremill.quicklens.PathModify


/**
  * Created by marius on 23/05/17.
  */


object Component {
  type Reducer[State] = PartialFunction[(State, Action), State]
  type ReducerFull[State] = (State, Action) =>  State

}


trait Component {

  type State

  val reducer: Reducer

  type Reducer = Component.Reducer[State]
  type ReducerFull = Component.ReducerFull[State]

  @inline def reducerFull: ReducerFull = reducer.full

  private implicit class toFullReducer[S](reducer: Component.Reducer[S]) {
    private val ignoreActionReducer: Component.ReducerFull[S] = (s, _) => s
    @inline def full: Component.ReducerFull[S] = (s,a) => reducer.applyOrElse((s,a), ignoreActionReducer.tupled)
  }

  protected def combineReducersFirst(reducers: Reducer*): Reducer = reducers.reduce(_ orElse _)

  protected def combineReducers(reducers: Reducer*): Reducer = {
    case (state, act) => reducers.foldLeft(state)((s, reducer) => reducer.full(s, act))
  }

  protected def subReducer[S, SS](reducer: Component.Reducer[SS], modifyState: S => PathModify[S, SS]): Component.Reducer[S] = {
    case (s, a) => modifyState(s).using(reducer.full(_, a))
  }
}

trait EffectsComponent extends Component with Effects