package outwatch.extras

import com.softwaremill.quicklens.PathModify


/**
  * Created by marius on 23/05/17.
  */


object Component {

  type Reducer[M] = PartialFunction[(M, Action), M]
  type ReducerFull[M] = (M, Action) =>  M

  implicit class toFullReducer[M](reducer: Reducer[M]) {
    private val ignoreActionReducer: ReducerFull[M] = (s, _) => s

    def full: ReducerFull[M] = (s,a) => reducer.applyOrElse((s,a), ignoreActionReducer.tupled)
  }
}


trait Component {
  import Component.{ Reducer => ReducerM, _}

  type Model
  type Reducer = ReducerM[Model]
  type ReducerFull = (Model, Action) =>  Model

  val reducer: Reducer

  protected def combineReducersFirst(reducers: Reducer*): Reducer = reducers.reduce(_ orElse _)

  protected def combineReducers(reducers: Reducer*): Reducer = {
    case (state, act) => reducers.foldLeft(state)((s, reducer) => reducer.full(s, act))
  }

  protected def subReducer[M, S](reducer: ReducerM[S], modifyState: M => PathModify[M, S]): ReducerM[M] = {
    case (s, a) => modifyState(s).using(reducer.full(_, a))
  }
}
