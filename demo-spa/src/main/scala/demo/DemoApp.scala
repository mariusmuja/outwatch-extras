package demo

import demo.styles._
import org.scalajs.dom
import org.scalajs.dom.console
import outwatch.Sink
import outwatch.dom.VNode
import outwatch.redux._
import outwatch.router.{BaseUrl, Router => OutwatchRouter}
import outwatch.styles.Styles
import rxscalajs.Observable

import scala.scalajs.js.{Date, JSApp}
import scala.util.Random
import scalacss.DevDefaults._



object Logger extends Component with
                      LogAreaStyle {
  sealed trait Action
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

  def view(store: Store[State, Action])(implicit stl: Style): VNode = {
    import outwatch.dom._

    div(
      div(
        input(value <-- store.map(_.log.lastOption.getOrElse(""))),
        button(
          click --> Router.replace(Router.LogPage(1)), "Goto"
        )
      ),
      div(
        textarea(stl.textfield, stl.material,
          child <-- store.map(_.log.mkString("\n"))
        )
      )
    )
  }

  def apply(initActions: Action*): VNode = {
    view(createStore(initActions))
  }

  def withSink(initActions: Action*): (VNode, ActionSink) = {

    val consoleActions = Console.sourceMerge.map {
      case Console.Output(str) => LogAction(str)
    }

    val store = createStore(initActions, consoleActions)
    (view(store), store.sink)
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

  sealed trait Action
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

  def view(store: Store[State, Action], logger: Logger.ActionSink, parent: TodoComponent.ActionSink)(implicit stl: Style): VNode = {
    import outwatch.dom._

    val loggedActions = logger.redirectMap[Action]{
      case AddTodo(value) => Logger.LogAction(s"Add $value")
      case RemoveTodo(todo) => Logger.LogAction(s"Remove ${todo.value}")
    }
    val parentSink = parent.redirectMap[Action] {
      case AddTodo(value) => TodoComponent.AddTodo(value)
      case RemoveTodo(todo) => TodoComponent.RemoveTodo(todo.value)
    }
    val consoleSink = Console.sink.redirectMap[Action] {
      case AddTodo(value) => Console.Log(value)
      case RemoveTodo(todo) => Console.Log(todo.value)
    }

    val actions = SinkUtil.redirectInto(store.sink, loggedActions, parentSink, consoleSink)

    val todoViews = store.source.map(_.todos.map(todoItem(_, actions, stl)))

    div(
      TextField(actions.redirectMap(AddTodo)),
      button(stl.button, stl.material,
        click --> Router.set(Router.LogPage(10)), "Log only"
      ),
      ul(children <-- todoViews)
    )
  }

  def apply(logger: Logger.ActionSink,
            parent: TodoComponent.ActionSink,
            initActions: Action*): VNode = {
    view(createStore(initActions), logger, parent)
  }
}

object TodoComponent extends Component {

  sealed trait Action
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

  def view(store: Store[State, Action]): VNode = {
    import outwatch.dom._

    val (logger, loggerSink) = Logger.withSink()

    table(
      tbody(
        tr(
          td("Last action: ", child <-- store.map(_.lastAction))
        ),
        tr(
          td(TodoModule(loggerSink, store.sink)),
          td(TodoModule(loggerSink, store.sink))
        ),
        tr(
          td(logger)
        )
      )
    )
  }

  def apply(initActions: Action*): VNode = {
    view(createStore(initActions))
  }

}



object Router extends OutwatchRouter {

  sealed trait Page
  object TodoPage extends Page
  case class LogPage(last: Int) extends Page

  val baseUrl: BaseUrl = BaseUrl.until_# + "#"

  override def baseLayout(node: Observable[VNode]) : VNode = {
    import outwatch.dom._
    div(
      h4("Todo"),
      div(outwatch.dom.child <-- node)
    )
  }

  val config = RouterConfig { builder =>
    import builder._

    builder.rules(
      ("log" / int).caseClass[LogPage] ~> { case LogPage(p) => Logger(Logger.Init("Init logger: " + p)) },
      "todo".const(TodoPage) ~> TodoComponent(),
      "log".const(Unit) ~> Redirect(LogPage(11), replace = true)
    )
      .notFound(Redirect(TodoPage, replace = true))
  }


  override def onPageChange = {
    case TodoPage => dom.document.title = "TODO list"
    case LogPage(_) => dom.document.title = "Log page"
  }
}


object Console extends Effects {

  sealed trait Effect
  case class Log(str: String) extends Effect

  sealed trait EffectResult
  case class Output(str: String) extends EffectResult

  def effects: Effect => Observable[EffectResult] = {
    case Log(str) =>
      Observable.just(Output(str)).delay(1000)
  }
}


object DemoApp{
  import outwatch.dom.OutWatch

  def main(args: Array[String]): Unit = {
    Styles.subscribe(_.addToDocument())

    OutWatch.render("#app", Router())
  }
}
