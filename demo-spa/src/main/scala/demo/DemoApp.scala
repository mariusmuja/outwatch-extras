package demo

import demo.styles._
import org.scalajs.dom
import org.scalajs.dom.{Event, EventTarget, console}
import outwatch.{Sink, SinkUtil}
import outwatch.dom.{Handlers, VNode}
import outwatch.extras._
import outwatch.extras.router._
import outwatch.styles.Styles
import rxscalajs.Observable
import rxscalajs.Observable.Creator

import scala.concurrent.duration._
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.{Date, JSApp}
import scala.util.Random
import scalacss.DevDefaults._



object Logger extends Component with
                      LogAreaStyle {
  case class Init(message: String) extends Action
  case class LogAction(action: String) extends Action

  private def now = (new Date).toLocaleString()

  case class State(
    log: Seq[String] = Seq("Log:")
  ) extends ComponentState {

    def evolve = {
      case Init(message) =>
        copy(log :+ message)
      case LogAction(line) =>
        console.log(s"Log >>>> $line")
        copy(log :+ s"$now : $line")
    }
  }

  def init = State()

  def view(store: Store[State, Action], router: Router.PageSink)(implicit stl: Style): VNode = {
    import outwatch.dom._

    div(
      div(
        input(value <-- store.map(_.log.lastOption.getOrElse(""))),
        button(
          click(Router.LogPage(1).withReplace) --> router, "Goto"
        )
      ),
      div(
        textarea(stl.textfield, stl.material,
          child <-- store.map(_.log.mkString("\n"))
        )
      )
    )
  }

  def apply(router: Router.PageSink, initActions: Action*): VNode = {
    view(mkStore(initActions), router)
  }

  def withSink(router: Router.PageSink, initActions: Action*): (VNode, ActionSink) = {
    val store = mkStore(initActions)
    view(store, router) -> store.sink
  }
}

object TextField extends TextFieldStyle {

  def apply(actions: Sink[String], minLen : Int = 4)(implicit stl: Style): VNode = {
    import outwatch.dom._

    val inputTodo = createStringHandler()

    val disabledValues = inputTodo
      .map(_.length < minLen)
      .startWith(true)

    val filterSinkDisabled = (act: Observable[String]) =>
      act.withLatestFrom(disabledValues)
        .filter(x => !x._2)
        .map(_._1)

    val filteredActions = actions.redirect(filterSinkDisabled)
    val inputTodoFiltered = inputTodo.redirect(filterSinkDisabled)

    val enterdown = keydown.filter(k => k.keyCode == 13)

    div(
      div(stl.textfield, stl.material,
        label(stl.textlabel, "Enter todo"),
        input(stl.textinput,
          inputString --> inputTodo,
          value <-- inputTodo,
          enterdown(inputTodo) --> filteredActions,
          enterdown("") --> inputTodoFiltered
        )
      ),
      button(stl.button, stl.material,
        click(inputTodo) --> filteredActions,
        click("") --> inputTodoFiltered,
        disabled <-- disabledValues,
        "Submit"
      )
    )
  }

}


object TodoModule extends Component with
                          TodoModuleStyle {

  case class AddTodo(value: String) extends Action
  case class RemoveTodo(todo: Todo) extends Action


  private def newID = Random.nextInt

  case class Todo(id: Int, value: String)

  case class State(todos: Seq[Todo] = Seq.empty) extends ComponentState {
    def evolve = {
      case AddTodo(value) =>
        copy(todos = todos :+ Todo(newID, value))
      case RemoveTodo(todo) =>
        copy(todos = todos.filter(_.id != todo.id))
    }
  }

  private def todoItem(todo: Todo, actions: ActionSink, stl: Style): VNode = {
    import outwatch.dom._
    li(
      key := s"${todo.id}",
      span(todo.value),
      button(stl.button, stl.material, click(RemoveTodo(todo)) --> actions, "Delete")
    )
  }

  def init = State()

  def view(store: Store[State, Action], routes: Router.PageSink, logger: Logger.ActionSink, parent: TodoComponent.ActionSink)(implicit stl: Style): VNode = {
    import outwatch.dom._

    val loggedActions = logger.redirectMap[Action]{
      case AddTodo(value) => Logger.LogAction(s"Add $value")
      case RemoveTodo(todo) => Logger.LogAction(s"Remove ${todo.value}")
    }
    val parentSink = parent.redirectMap[Action] {
      case AddTodo(value) => TodoComponent.AddTodo(value)
      case RemoveTodo(todo) => TodoComponent.RemoveTodo(todo.value)
    }

    val actions = SinkUtil.redirectInto(store.sink, loggedActions, parentSink)

    val todoViews = store.source.map(_.todos.map(todoItem(_, actions, stl)))

    div(
      TextField(actions.redirectMap(AddTodo)),
      button(stl.button, stl.material,
        click(Router.LogPage(10)) --> routes, "Log only"
      ),
      ul(children <-- todoViews)
    )
  }

  def apply(router: Router.PageSink,
            logger: Logger.ActionSink,
            parent: TodoComponent.ActionSink,
            initActions: Action*): VNode = {
    view(mkStore(initActions), router, logger, parent)
  }
}

object TodoComponent extends Component {

  case class AddTodo(value: String) extends Action
  case class RemoveTodo(value: String) extends Action

  case class State(
    lastAction: String = "None"
  ) extends ComponentState {

    def evolve = {
      case AddTodo(value) => copy(lastAction = s"Add $value")
      case RemoveTodo(value) => copy(lastAction = s"Remove $value")
    }
  }

  def init = State()

  def view(store: Store[State, Action], router: Sink[Router.Page]): VNode = {
    import outwatch.dom._

    val (logger, loggerSink) = Logger.withSink(router)

    table(
      tbody(
        tr(
          td("Last action: ", child <-- store.map(_.lastAction))
        ),
        tr(
          td(TodoModule(router, loggerSink, store.sink)),
          td(TodoModule(router, loggerSink, store.sink))
        ),
        tr(
          td(logger)
        )
      )
    )
  }

  def apply(router: Router.PageSink, initActions: Action*): VNode = {
    view(mkStore(initActions), router)
  }

}



object Router extends Router {

  object TodoPage extends Page
  case class LogPage(last: Int) extends Page

  val baseUrl = BaseUrl.until_# + "#"

  override def baseLayout(node: Observable[VNode]) : VNode = {
    import outwatch.dom._
    div(
      h4("Todo"),
      div(outwatch.dom.child <-- node)
    )
  }

  val config = RouterConfig{ dsl =>
    import dsl._

    dsl.rules(
      ("log" / int).caseClass[LogPage] ~> { case LogPage(p) => Logger(routerActions, Logger.Init("Init logger: " + p)) },
      "todo".const(TodoPage) ~> TodoComponent(routerActions),
      "log".const(Unit) ~> LogPage(11).withReplace
    )
      .notFound(TodoPage.withReplace)
  }
}


object DemoApp extends JSApp {

  import outwatch.dom.OutWatch

  def main(): Unit = {
    Styles.subscribe(_.addToDocument())

    OutWatch.render("#app", Router())
  }
}
