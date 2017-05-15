package util

import com.softwaremill.quicklens._
import outwatch.Sink
import outwatch.dom._
import outwatch.util.Store
import rxscalajs.Observable

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

  def apply(sink: Sink[String]): VNode = {

    val inputTodo = createStringHandler()

    val disabledValues = inputTodo
      .map(_.length < 4)
      .startWith(true)

    val enterdown = keydown.filter(_.keyCode == 13)

    div(
      label("Todo: "),
      input(
        inputString --> inputTodo,
        value <-- inputTodo,
        enterdown(inputTodo) --> sink,
        enterdown("") --> inputTodo
      ),
      button(
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

  def reducerFull(state: State, act: Action) : State = reducer(state, act)

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
  val store = Store(MainComponent.State(), MainComponent.reducerFull)
  val source = store.source.share
  val sink = store.sink

  val root = MainComponent(source,sink)
}




object TestApp extends JSApp {

  def main() = {
    OutWatch.render("#app", RootModule.root)
  }
}
