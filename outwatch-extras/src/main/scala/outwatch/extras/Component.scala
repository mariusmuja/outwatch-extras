package outwatch.extras


/**
  * Created by marius on 23/05/17.
  */
trait Component {

  protected trait StateLike[State] {
    def evolve : PartialFunction[Action, State]
  }

  type ComponentState = StateLike[State]
  type State <: ComponentState

  val reducer: Reducer = {
    case (state, action) if state.evolve.isDefinedAt(action) => state.evolve(action)
  }

  def store(handler: Handler[Action], initState: State, initAction: Action): Store[State, Action] = {

    val source = handler.startWith(initAction)
      .scan(initState)(reducer.full)
      .publishReplay(1)
      .refCount

    Store(source, handler)
  }

  type Reducer = PartialFunction[(State, Action), State]
  type ReducerFull = (State, Action) =>  State

  private implicit class toFullReducer(reducer: Reducer) {
    private val ignoreActionReducer: ReducerFull = (s, _) => s
    @inline def full: ReducerFull = (s,a) => reducer.applyOrElse((s,a), ignoreActionReducer.tupled)
  }
}