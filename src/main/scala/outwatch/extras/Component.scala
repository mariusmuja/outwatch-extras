package outwatch.extras

import com.softwaremill.quicklens.PathModify


/**
  * Created by marius on 23/05/17.
  */


object Component {
  type Reducer[M] = PartialFunction[(M, Action), M]
  type ReducerFull[M] = (M, Action) =>  M

}


trait Component {

  type Model

  val reducer: Reducer

  type Reducer = Component.Reducer[Model]
  type ReducerFull = Component.ReducerFull[Model]

  @inline def reducerFull: ReducerFull = reducer.full

  private implicit class toFullReducer[M](reducer: Component.Reducer[M]) {
    private val ignoreActionReducer: Component.ReducerFull[M] = (s, _) => s
    @inline def full: Component.ReducerFull[M] = (s,a) => reducer.applyOrElse((s,a), ignoreActionReducer.tupled)
  }

  protected def combineReducersFirst(reducers: Reducer*): Reducer = reducers.reduce(_ orElse _)

  protected def combineReducers(reducers: Reducer*): Reducer = {
    case (state, act) => reducers.foldLeft(state)((s, reducer) => reducer.full(s, act))
  }

  protected def subReducer[M, S](reducer: Component.Reducer[S], modifyState: M => PathModify[M, S]): Component.Reducer[M] = {
    case (s, a) => modifyState(s).using(reducer.full(_, a))
  }
}

trait EffectsComponent extends Component with Effects
