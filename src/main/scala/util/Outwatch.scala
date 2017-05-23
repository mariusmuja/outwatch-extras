package util

import com.softwaremill.quicklens._
import org.scalajs.dom
import outwatch.Sink
import outwatch.dom._
import rxscalajs.subscription.Subscription
import rxscalajs.{Observable, Subject}
import util.StyleAdaptors._

import scala.scalajs.js.JSApp
import scala.util.Random
import scalacss.DevDefaults._


trait Action

object Component {

  type Reducer[M] = PartialFunction[(M, Action), M]
  type ReducerFull[M] = (M, Action) =>  M

  implicit class toFullReducer[M](reducer: Reducer[M]) {
    private val ignoreReducer: ReducerFull[M] = (s, _) => s

    def full: ReducerFull[M] = (s,a) => reducer.applyOrElse((s,a), ignoreReducer.tupled)
  }

}

trait Component {
  import Component.{ Reducer => ReducerM, _}

  type Model
  type Reducer = ReducerM[Model]
  type ReducerFull = (Model, Action) =>  Model

  type ActionSink = Sink[Action]
  type ModelObservable = Observable[Model]

  val reducer: Reducer


  protected def combineReducersFirst(reducers: Reducer*): Reducer = reducers.reduce(_ orElse _)

  protected def combineReducers(reducers: Reducer*): Reducer = {
    case (state, act) => reducers.foldLeft(state)((s, reducer) => reducer.full(s, act))
  }

  protected def subReducer[M, S](reducer: ReducerM[S], modifyState: M => PathModify[M, S]): ReducerM[M] = {
    case (s, a) => modifyState(s).using(reducer.full(_, a))
  }
}

object Effects {
  type EffectHandler = PartialFunction[Action, Observable[Action]]
  type EffectHandlerFull = Action => Observable[Action]

  implicit class toFullEffectHandler(handler: EffectHandler) {
    private val noEffect: EffectHandlerFull = _ => Observable.empty

    def full: EffectHandlerFull = handler.applyOrElse(_, noEffect)
  }

}

trait Effects {
  import Effects._
  type EffectHandler = Effects.EffectHandler
  type EffectHandlerFull = Effects.EffectHandlerFull

  val effects: EffectHandler

  def effectsFull: EffectHandlerFull = effects.full

  protected def combineEffectsFirst(handlers: EffectHandler*): EffectHandler = handlers.reduce(_ orElse _)

  protected def combineEffects(handlers: EffectHandler*): EffectHandler = {
    case action: Action => Observable.from(handlers).flatMap(_.full(action))
  }
}

object Styles {
  private val inner = Subject[StyleSheet.Inline]()

  def publish(ss: StyleSheet.Inline): Unit = inner.next(ss)

  def subscribe(f: StyleSheet.Inline => Unit): Subscription = inner.subscribe(f)

  trait Publisher { self: StyleSheet.Inline =>
    publish(self)
  }

}


object StyleAdaptors {
  import scala.language.implicitConversions

  implicit def styleToAttr(styleA: StyleA): Attribute = {
    cls := styleA.htmlClass
  }
}


object MdlStyles extends StyleSheet.Inline {

  import dsl._

  val textfield = mixin (
    addClassName("mdl-textfield mdl-js-textfield mdl-textfield--floating-label")
  )

  val textinput = mixin (
    addClassName("mdl-textfield__input")
  )

  val textlabel = mixin (
    addClassName("mdl-textfield__label")
  )

  val button = mixin (
    addClassName("mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect")
  )
}




object LogArea extends Component {
  case class LogAction(action: String) extends Action

  class Style extends StyleSheet.Inline {

    import dsl._

    val textfield = style(
      MdlStyles.textfield,
      height(400.px)
    )
  }

  object DefaultStyle extends Style with Styles.Publisher

  case class Model(
    log: Seq[String] = Seq()
  )

  val reducer: Reducer = {
    case (state, LogAction(line)) => modify(state)(_.log).using(_ :+ line)
  }

  def apply(source: ModelObservable, stl: Style = DefaultStyle): VNode = {
    textarea(stl.textfield,
      child <-- source.map(_.log.mkString("\n"))
    )
  }

}


object TextField {

  class Style extends StyleSheet.Inline {

    import dsl._

    val textfield = style (
      MdlStyles.textfield,
      marginRight(8.px).important
    )

    val textinput = style (
      MdlStyles.textinput
    )

    val textlabel = style (
      MdlStyles.textlabel
    )

    val button = style (
      MdlStyles.button
    )
  }

  object DefaultStyle extends Style with Styles.Publisher

  def apply(actions: Sink[String], stl: Style = DefaultStyle): VNode = {

    val inputTodo = createStringHandler()

    val disabledValues = inputTodo
      .map(_.length < 4)
      .startWith(true)

    val enterdown = keydown.filter(_.keyCode == 13)

    div(
      div(stl.textfield,
        label(stl.textlabel, "Enter todo"),
        input(stl.textinput,
          inputString --> inputTodo,
          value <-- inputTodo,
          enterdown(inputTodo) --> actions,
          enterdown("") --> inputTodo
        )
      ),
      button(stl.button,
        click(inputTodo) --> actions,
        click("") --> inputTodo,
        disabled <-- disabledValues,
        "Submit"
      )
    )
  }

}



object TodoModule extends Component with Effects {

  import LogArea.LogAction

  case class AddTodo(value: String) extends Action
  case class RemoveTodo(todo: Todo) extends Action

  case class Todo(id: Int, value: String)
  case class Model(todos: Seq[Todo] = Seq())

  private def newID = Random.nextInt

  val reducer: Reducer = {
    case (state, RemoveTodo(todo)) =>
      modify(state)(_.todos).using(_.filter(_.id != todo.id))
    case (state, AddTodo(value)) =>
      modify(state)(_.todos).using(_ :+ Todo(newID, value))
  }

  override val effects: EffectHandler = {
    case AddTodo(s) =>
      Observable.interval(Random.nextInt(500)).take(1).mapTo(LogAction(s"Add action: $s"))
    case RemoveTodo(todo) =>
      Observable.interval(Random.nextInt(500)).take(1).mapTo(LogAction(s"Remove action: ${todo.value}"))
  }


  class Style extends StyleSheet.Inline {

    import dsl._

    val textinput = style(
      MdlStyles.textinput
    )

    val textlabel = style(
      MdlStyles.textlabel
    )

    val button = style(
      MdlStyles.button,
      marginLeft(8.px)
    )
  }

  object DefaultStyle extends Style with Styles.Publisher

  def todoItem(todo: Todo, actions: ActionSink, stl: Style = DefaultStyle): VNode = {
    li(
      span(todo.value),
      button( stl.button,
        click(RemoveTodo(todo)) --> actions,
        "Delete"
      )
    )
  }

  def apply(model: ModelObservable, actions: ActionSink): VNode = {

    val stringSink = actions.redirect[String]{ item => item.map(AddTodo) }

    val todoViews = model
      .map(_.todos.map(todoItem(_, actions)))

    div(
      TextField(stringSink),
      ul(children <-- todoViews)
    )
  }

}


object MainComponent extends Component with Effects {
  import TodoModule.{AddTodo, RemoveTodo}

  case class Model(
    lastAction: String = "Start",
    todo: TodoModule.Model = TodoModule.Model(),
    log: LogArea.Model = LogArea.Model()
  )

  val lastActionReducer: Reducer = {
    case (model, AddTodo(_)) => model.modify(_.lastAction).using(_ => "Add")
    case (model, RemoveTodo(_)) => model.modify(_.lastAction).using(_ => "Remove")
  }

  val reducer: Reducer = combineReducers(
    lastActionReducer,
    subReducer(TodoModule.reducer, modify(_)(_.todo)),
    subReducer(LogArea.reducer, modify(_)(_.log))
  )

  val effects: EffectHandler = combineEffects(
    TodoModule.effects
  )

  def apply(model: ModelObservable, actions: ActionSink): VNode = {
    table(
      tbody(
        tr(
          td("Last action: ", child <-- model.map(_.lastAction))
        ),
        tr(
          td(TodoModule(model.map(_.todo), actions))
        ),
        tr(
          td(LogArea(model.map(_.log)))
        )
      )
    )
  }
}


object RootModule {


  private val initialState = MainComponent.Model()
  private val actions = createHandler[Action]()
  actions <-- actions.flatMap(MainComponent.effectsFull)


//  val t = MainComponent.reducer.full
//  val s = MainComponent.effects.full
  private val model = actions
    .scan(initialState)(MainComponent.reducer.full)
    .startWith(initialState)
    .share


  val root = MainComponent(model, actions)
}




object TestApp extends JSApp {
  Styles.subscribe(_.addToDocument())

  def main(): Unit = {
    OutWatch.render("#app", RootModule.root)
  }
}
