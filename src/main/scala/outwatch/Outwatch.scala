package util

import com.softwaremill.quicklens._
import outwatch.Sink
import outwatch.dom._
import outwatch.util.Store
import rxscalajs.Observable

import scalacss.DevDefaults._
import scalacss.internal.mutable.GlobalRegistry
import scala.scalajs.js.JSApp
import scala.util.Random


sealed trait Action

object Module {
  type Reducer[State, Action] = PartialFunction[(State, Action), State]
}

trait Module {

  type State
  type Reducer = Module.Reducer[State, Action]
  type ActionSink = Sink[Action]

  val reducer: Reducer
}



case class LogAction(action: String) extends Action
object LogModule extends Module {

  case class State(log: Seq[String] = Seq())

  val reducer : Reducer = {
    case (state, LogAction(line)) => state.modify(_.log).using(_ :+ line)
  }

  def apply(log: Observable[State]): VNode = {
    textarea(cols := 40, rows := 20, child <-- log.map(_.log.mkString("\n")))
  }

}


object TextField {


  class Style(implicit r: StyleSheet.Register) extends StyleSheet.Inline()(r) {

    import dsl._

    val textfield = style (
      addClassName("mdl-textfield mdl-js-textfield mdl-textfield--floating-label"),
      paddingRight(8.px)
    )

    val textinput = style (
      addClassName("mdl-textfield__input")
    )

    val textlabel = style (
      addClassName("mdl-textfield__label")
    )

    val button = style (
      addClassName("mdl-button mdl-js-button mdl-button--raised")
    )
  }

  object Style {
    lazy val default = {
      val style = new Style
      GlobalRegistry.register(style)
      style
    }
  }


  implicit def styleToAttr(styleA: StyleA): Attribute = {
    cls := styleA.htmlClass
  }


  def apply(sink: Sink[String], style: Style = Style.default): VNode = {

    val inputTodo = createStringHandler()

    val disabledValues = inputTodo
      .map(_.length < 4)
      .startWith(true)

    val enterdown = keydown.filter(_.keyCode == 13)

    div(
      div(style.textfield,
        label(style.textlabel, "Enter todo"),
        input(style.textinput,
          inputString --> inputTodo,
          value <-- inputTodo,
          enterdown(inputTodo) --> sink,
          enterdown("") --> inputTodo
        )
      ),
      button(style.button,
        click(inputTodo) --> sink,
        click("") --> inputTodo,
        disabled <-- disabledValues,
        "Submit"
      )
    )
  }

}


object TodoModule extends Module {

  case class AddTodo(value: String) extends Action
  case class RemoveTodo(id: Int) extends Action

  case class Todo(id: Int, value: String)
  case class State(todos: Seq[Todo] = Seq())

  private def newID = Random.nextInt


  val reducer: Reducer = {
    case (state, RemoveTodo(id)) => state.modify(_.todos).using(_.filter(_.id != id))
    case (state, AddTodo(value)) => state.modify(_.todos).using(_ :+ Todo(newID, value))
  }

  def todoComponent(todo: TodoModule.Todo, sink: Sink[Action], log: Sink[LogAction]): VNode = {
    li(
      span(todo.value),
      button(
        click(LogAction(s"Remove action ${todo.value }")) --> log,
        click(TodoModule.RemoveTodo(todo.id)) --> sink,
        "Delete"
      )
    )
  }

  def apply(todo: Observable[State], sink: Sink[Action]): VNode = {

    val stringSink = sink.redirect[String]{ item =>
      item.flatMap { s =>
        Observable.just(
          LogAction(s"Add action: $s"),
          AddTodo(s)
        )
      }
    }

    val todoViews = todo
      .map(_.todos.map(todoComponent(_, sink, sink)))

    div(
      TextField(stringSink),
      ul(children <-- todoViews)
    )
  }
}


object MainComponent extends Module {

  case class State(
    todo: TodoModule.State = TodoModule.State(),
    log: LogModule.State = LogModule.State()
  )

  val reducer : Reducer = {
    case (state, act) if TodoModule.reducer.isDefinedAt((state.todo, act)) =>
      state.modify(_.todo).using(TodoModule.reducer(_, act))
    case (state, act) if LogModule.reducer.isDefinedAt((state.log, act)) =>
      state.modify(_.log).using(LogModule.reducer(_, act))
    case (state, _) => state
  }

  def apply(source: Observable[State], sink: Sink[Action]): VNode = {
    table(
      tbody(
        tr(
          td(TodoModule(source.map(_.todo), sink)),
          td(LogModule(source.map(_.log)))
        )
      )
    )
  }
}


object RootModule {
  val initialState = MainComponent.State()
  val sink = createHandler[Action]()
  val source = sink
    .scan(initialState)((s, a) => MainComponent.reducer(s, a))
    .startWith(initialState)
    .share

  val root = MainComponent(source,sink)
}




object TestApp extends JSApp {

  def main() = {
    GlobalRegistry.addToDocumentOnRegistration()
    OutWatch.render("#app", RootModule.root)
  }
}
