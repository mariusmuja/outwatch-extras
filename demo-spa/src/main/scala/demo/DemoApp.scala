package demo

import cats.effect.IO
import demo.Router.{LogPage, Page, TodoPage}
import demo.styles._
import monix.execution.Ack, Ack.Continue
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom
import outwatch.dom.{Observable, Sink, VNode}
import outwatch.redux._
import outwatch.router.{BaseUrl, Router => OutwatchRouter}
import outwatch.styles.Styles

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.js.Date
import scala.util.Random
import scalacss.DevDefaults._



object Logger extends StatefulEffectsComponent with LogAreaStyle {

  sealed trait Action
  case class Init(message: String) extends Action
  case class InitEffect(message: String) extends Action
  case class LogAction(action: String) extends Action

  private def now = (new Date).toLocaleString()

  type Effect = ConsoleEffect

  case class State(
    log: Seq[String] = Seq("Log:")
  ) extends ComponentState {

    val evolve = {
      case InitEffect(message) =>
        ConsoleEffect.Log(message)
      case Init(message) =>
        copy(log :+ message)
      case LogAction(line) =>
        dom.console.log(s"Log >>>> $line")
        copy(log :+ s"$now : $line")
    }
  }

  def view(handler: Action >--> State)(implicit S: Style): VNode = {
    import outwatch.dom._


    div(
      div(
        input(value <-- handler.source.map(_.log.lastOption.getOrElse(""))),
        button(
          click(Router.LogPage(1)) --> Router.replace, "Goto"
        )
      ),
      div(
        textarea(S.textfield, S.material,
          child <-- handler.source.map(_.log.mkString("\n"))
        )
      )
    )
  }

  private val consoleToAction: ConsoleEffectResult => Action = {
    case ConsoleEffectResult.Output(str) => LogAction(str)
  }

  def apply(initActions: Action*): VNode = {
    Store.create(initActions, State(), Console.Merge.map(consoleToAction))
      .flatMap(view)
  }

  def withSink(initActions: Action*): IO[(VNode, ActionSink)] = {

    Store.create(initActions, State(), Console.Merge.map(consoleToAction)).map { store =>
      (view(store), store.sink)
    }
  }
}

object TextField extends TextFieldStyle {

  def apply(actions: Sink[String], minLen : Int = 4)(implicit S: Style): VNode = {
    import outwatch.dom._

    createStringHandler().flatMap { inputTodo =>

      val disabledValues = inputTodo
        .map(_.length < minLen)
        .startWith(Seq(true))

      val filterSinkDisabled = (act: Observable[String]) =>
        act.withLatestFrom(disabledValues)((a,b) => (a,b))
          .filter(x => !x._2)
          .map(_._1)

      val filteredActions = actions.redirect(filterSinkDisabled)
      val inputTodoFiltered = inputTodo.redirect(filterSinkDisabled)

      val enterdown = keydown.filter(k => k.keyCode == 13)

      div(
        div(S.textfield, S.material,
          label(S.textlabel, "Enter todo"),
          input(S.textinput,
            inputString --> inputTodo,
            value <-- inputTodo,
            enterdown(inputTodo) --> filteredActions,
            enterdown("") --> inputTodoFiltered
          )
        ),
        button(S.button, S.material,
          click(inputTodo) --> filteredActions,
          click("") --> inputTodoFiltered,
          disabled <-- disabledValues,
          "Submit"
        )
      )
    }
  }

}


object TodoModule extends StatefulComponent with
                          TodoModuleStyle {

  sealed trait Action
  case class AddTodo(value: String) extends Action
  case class RemoveTodo(todo: Todo) extends Action


  private def newID = Random.nextInt

  case class Todo(id: Int, value: String)

  case class State(todos: Seq[Todo] = Seq.empty) extends ComponentState {
    val evolve = {
      case add @ AddTodo(value) =>
        copy(todos = todos :+ Todo(newID, value))
      case remove @ RemoveTodo(todo) =>
        copy(todos = todos.filter(_.id != todo.id))
    }
  }

  private def todoItem(todo: Todo, actions: ActionSink, S: Style): VNode = {
    import outwatch.dom._
    li(
      key := s"${todo.id}",
      span(todo.value),
      button(S.button, S.material, click(RemoveTodo(todo)) --> actions, "Delete")
    )
  }

  def view(store: Action >--> State,  logger: Logger.ActionSink, parent: TodoComponent.ActionSink)(implicit S: Style): VNode = {
    import outwatch.dom._

    val loggerSink = logger.redirectMap[Action]{
      case AddTodo(value) => Logger.LogAction(s"Add $value")
      case RemoveTodo(todo) => Logger.LogAction(s"Remove ${todo.value}")
    }
    val parentSink = parent.redirectMap[Action] {
      case AddTodo(value) => TodoComponent.AddTodo(value)
      case RemoveTodo(todo) => TodoComponent.RemoveTodo(todo.value)
    }
    val consoleSink = Console.sink.redirectMap[Action] {
      case AddTodo(value) => ConsoleEffect.Log(value)
      case RemoveTodo(todo) => ConsoleEffect.Log(todo.value)
    }

    SinkUtil.redirectInto(store.sink, loggerSink, parentSink, consoleSink).flatMap { actions =>

      val todoViews = store.source.map(_.todos.map(todoItem(_, actions, S)))

      div(
        TextField(actions.redirectMap(AddTodo)),
        button(S.button, S.material,
          click(Router.LogPage(10)) --> Router.set, "Log only"
        ),
        ul(children <-- todoViews)
      )
    }
  }

  def apply(logger: Logger.ActionSink,
    parent: TodoComponent.ActionSink,
    initActions: Action*
  ): VNode = {
    Store.create(initActions, State()).flatMap(view(_, logger, parent))
  }
}

object TodoComponent extends StatefulComponent {

  sealed trait Action
  case class AddTodo(value: String) extends Action
  case class RemoveTodo(value: String) extends Action

  case class State(
    lastAction: String = "None"
  ) extends ComponentState {

    val evolve = {
      case AddTodo(value) => copy(lastAction = s"Add $value")
      case RemoveTodo(value) => copy(lastAction = s"Remove $value")
    }
  }

  def view(store: Action >--> State): VNode = {
    import outwatch.dom._

    Logger.withSink(Logger.InitEffect("Effect log"))
      .flatMap { case (logger, loggerSink) =>

        val todoModule = TodoModule(loggerSink, store.sink)

        table(
          tbody(
            tr(
              td("Last action: ", child <-- store.source.map(_.lastAction))
            ),
            tr(
              td(todoModule),
              td(todoModule)
            ),
            tr(
              td(logger)
            )
          )
        )
      }
  }

  def apply(initActions: Action*): VNode = {
    Store.create(initActions, State())
      .flatMap(view)
  }

}



object Router extends OutwatchRouter {

  sealed trait Page
  object TodoPage extends Page
  case class LogPage(last: Int) extends Page

  val baseUrl: BaseUrl = BaseUrl.until_# + "#"

  val config = RouterConfig { builder =>
    import builder._

    builder
      .rules(
        ("log" / int).caseClass[LogPage] ~> { case LogPage(p) => Logger(Logger.Init("Init logger: " + p)) },
        "todo".const(TodoPage) ~> TodoComponent(),
        "log".const(Unit) ~> Redirect(LogPage(11), replace = true)
      )
      .notFound(Redirect(TodoPage, replace = true))
  }
}

sealed trait ConsoleEffect
object ConsoleEffect {
  case class Log(str: String) extends ConsoleEffect
}

sealed trait ConsoleEffectResult
object ConsoleEffectResult {
  case class Output(str: String) extends ConsoleEffectResult
}

object Console extends Effects[ConsoleEffect, ConsoleEffectResult] {
  import ConsoleEffect._
  import ConsoleEffectResult._

  val effects: ConsoleEffect => Observable[ConsoleEffectResult] = {
    case Log(str) =>
      dom.console.log(s"In console: $str")
      val obs = Observable(Output(str))
      obs.delayOnNext(1000.millis)
  }
}


object BaseLayout {

  def apply(node: Observable[VNode]): VNode = {
    import outwatch.dom._
    div(
      h4("Todo"),
      div(outwatch.dom.child <-- node)
    )
  }
}


object DemoApp {

  import outwatch.dom.OutWatch

  val updatePageTitle : Page => Future[Ack] = {
    case TodoPage =>
      dom.document.title = "TODO list"
      Continue
    case LogPage(_) =>
      dom.document.title = "Log page"
      Continue
  }

  def main(args: Array[String]): Unit = {

    Router.pageChanged.subscribe(updatePageTitle)

    Styles.subscribe(_.addToDocument())

    OutWatch.render("#app", Router(BaseLayout.apply)).unsafeRunSync()
  }
}
