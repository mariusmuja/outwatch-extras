package util

import com.softwaremill.quicklens._
import outwatch.Sink
import outwatch.dom._
import rxscalajs.subscription.Subscription
import rxscalajs.{Observable, Subject}
import util.StyleAdaptors._

import scala.scalajs.js.JSApp
import scala.util.Random
import scalacss.DevDefaults._


trait Action

trait Module {
  type State
  type Reducer = PartialFunction[(State, Action), State]

  type ActionSink = Sink[Action]
  type StateObservable = Observable[State]

  val reducer: Reducer

  def reducerFull(s: State, a: Action): State = reducer.applyOrElse((s, a), (x: (State, Action)) => x._1)
}

trait Effects {
  type EffectHandler = PartialFunction[Action, Observable[Action]]

  val effects: EffectHandler = PartialFunction.empty

  def effectsFull(a: Action): Observable[Action] = effects.applyOrElse(a, (_:Action) => Observable.empty)
}


object Styles {
  private val inner = Subject[StyleSheet.Inline]()

  def publish(ss: StyleSheet.Inline): Unit = inner.next(ss)

  def subscribe(f: StyleSheet.Inline => Unit): Subscription = inner.subscribe(f)

  trait Publisher { self : StyleSheet.Inline =>
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
    addClassName("mdl-button mdl-js-button mdl-button--raised")
  )
}




object LogModule extends Module {
  case class LogAction(action: String) extends Action

  class Style extends StyleSheet.Inline {

    import dsl._

    val textfield = style (
      MdlStyles.textfield,
      height(400.px)
    )
  }

  object Style {
    object default extends Style with Styles.Publisher
  }

  case class State(
    log: Seq[String] = Seq()
  )

  val reducer: Reducer = {
    case (state, LogAction(line)) => modify(state)(_.log).using(_ :+ line)
  }

  def apply(source: StateObservable, stl: Style = Style.default): VNode = {
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

  object Style {
    object default extends Style with Styles.Publisher
  }


  def apply(sink: Sink[String], stl: Style = Style.default): VNode = {

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
          enterdown(inputTodo) --> sink,
          enterdown("") --> inputTodo
        )
      ),
      button(stl.button,
        click(inputTodo) --> sink,
        click("") --> inputTodo,
        disabled <-- disabledValues,
        "Submit"
      )
    )
  }

}



object TodoModule extends Module with Effects {

  import LogModule.LogAction

  case class AddTodo(value: String) extends Action
  case class RemoveTodo(todo: Todo) extends Action

  case class Todo(id: Int, value: String)
  case class State(todos: Seq[Todo] = Seq())

  private def newID = Random.nextInt


  val reducer: Reducer = {
    case (state, RemoveTodo(todo)) => modify(state)(_.todos).using(_.filter(_.id != todo.id))
    case (state, AddTodo(value)) => modify(state)(_.todos).using(_ :+ Todo(newID, value))
  }

  override val effects: EffectHandler = {
    case AddTodo(s) =>
      Observable.just(LogAction(s"Add action: $s"))
    case RemoveTodo(todo) =>
      Observable.just(LogAction(s"Remove action: ${todo.value}"))
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

  object Style {
    object default extends Style with Styles.Publisher
  }

  def todoComponent(todo: Todo, sink: ActionSink, stl: Style = Style.default): VNode = {
    li(
      span(todo.value),
      button( stl.button,
        click(RemoveTodo(todo)) --> sink,
        "Delete"
      )
    )
  }

  def apply(source: StateObservable, sink: ActionSink): VNode = {

    val stringSink = sink.redirect[String]{ item => item.map(AddTodo) }

    val todoViews = source
      .map(_.todos.map(todoComponent(_, sink)))

    div(
      TextField(stringSink),
      ul(children <-- todoViews)
    )
  }

}


object MainComponent extends Module with Effects {

  case class State(
    todo: TodoModule.State = TodoModule.State(),
    log: LogModule.State = LogModule.State()
  )

  val reducer : Reducer = {
    case (state, act) if TodoModule.reducer.isDefinedAt((state.todo, act)) =>
      modify(state)(_.todo).using(TodoModule.reducer(_, act))
    case (state, act) if LogModule.reducer.isDefinedAt((state.log, act)) =>
      modify(state)(_.log).using(LogModule.reducer(_, act))
  }

//   combine(state.todo -> TodoModule.reducer, state.log -> LogModule.reducer)

  override val effects : EffectHandler = {
    case obs if TodoModule.effects.isDefinedAt(obs) => TodoModule.effects(obs)
  }


  def apply(source: StateObservable, sink: ActionSink): VNode = {
    table(
      tbody(
        tr(
          td(TodoModule(source.map(_.todo), sink))
        ),
        tr(
          td(LogModule(source.map(_.log)))
        )
      )
    )
  }
}


object RootModule {


  val initialState = MainComponent.State()
  val sink = createHandler[Action]()
  sink <-- sink.flatMap(MainComponent.effectsFull)

  val source = sink
    .scan(initialState)(MainComponent.reducerFull)
    .startWith(initialState)
    .share

  Styles.subscribe(_.addToDocument())

  val root = MainComponent(source, sink)
}




object TestApp extends JSApp {

  def main(): Unit = {
    OutWatch.render("#app", RootModule.root)
  }
}
