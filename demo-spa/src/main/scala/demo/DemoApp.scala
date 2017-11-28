package demo

import cats.effect.IO
import demo.styles._
import monix.execution.Ack.Continue
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalajs.dom
import outwatch.dom.{Observable, Sink, VNode}
import outwatch.extras.{<--<, >-->}
import outwatch.redux._
import outwatch.router.{AbsUrl, BaseUrl}
import outwatch.styles.Styles

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
        this -> ConsoleEffect.Log(message)
      case Init(message) =>
        copy(log :+ message)
      case LogAction(line) =>
        dom.console.log(s"Log >>>> $line")
        copy(log :+ s"$now : $line")
    }
  }

  def view(handler: State <--< Action)(implicit S: Style): VNode = {
    import AppRouter._
    import outwatch.dom._

    router.flatMap { router =>
      div(
        div(
          input(value <-- handler.map(_.log.lastOption.getOrElse(""))),
          button(
            click(Router.Replace(LogPage("replaced previous page"))) --> router, "Goto"
          )
        ),
        div(
          textarea(S.textfield, S.material,
            child <-- handler.map(_.log.mkString("\n"))
          )
        )
      )
    }
  }

  private val consoleToAction: ConsoleEffectResult => Action = {
    case ConsoleEffectResult.Output(str) => LogAction(str)
  }

  def apply(initActions: Action*): VNode = {
    Store.create(initActions, State(), Console.merge.map(_.mapSource(consoleToAction)))
      .flatMap(view)
  }

  def withSink(initActions: Action*): IO[(VNode, ActionSink)] = {

    Store.create(initActions, State(), Console.merge.map(_.mapSource(consoleToAction))).map { store =>
      (view(store), store)
    }
  }
}

object TextField extends TextFieldStyle {

  def apply(actions: Sink[String], minLen : Int = 4)(implicit S: Style): VNode = {
    import outwatch.dom._

    Handler.create[String].flatMap { inputTodo =>

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
            prop("value") <-- inputTodo,
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


object TodoModule extends StatefulEffectsComponent with
                          TodoModuleStyle {

  sealed trait Action
  case class AddTodo(value: String) extends Action
  case class RemoveTodo(todo: Todo) extends Action

  import AppRouter._

  type Effect = Router.Action

  private def newID = Random.nextInt

  case class Todo(id: Int, value: String)

  case class State(todos: Seq[Todo] = Seq.empty) extends ComponentState {
    val evolve = {
      case add @ AddTodo(value) =>
        if (value == "show log") {
          this -> Router.Push(LogPage("Log as effect"))
        } else
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
    import AppRouter._
    import outwatch.dom._

    val loggerSink = logger.redirectMap[Action]{
      case AddTodo(value) => Logger.LogAction(s"Add $value")
      case RemoveTodo(todo) => Logger.LogAction(s"Remove ${todo.value}")
    }
    val parentSink = parent.redirectMap[Action] {
      case AddTodo(value) => TodoComponent.AddTodo(value)
      case RemoveTodo(todo) => TodoComponent.RemoveTodo(todo.value)
    }

    SinkUtil.redirectInto(store, loggerSink, parentSink).flatMap { actions =>

      val todoViews = store.map(_.todos.map(todoItem(_, actions, S)))

      AppRouter.push.flatMap { router =>
        div(
          TextField(actions.redirectMap(AddTodo)),
          button(S.button, S.material,
            click(LogPage("from 'Log only'")) --> router, "Log only"
          ),
          ul(children <-- todoViews)
        )
      }
    }

  }

  def apply(logger: Logger.ActionSink,
    parent: TodoComponent.ActionSink,
    initActions: Action*
  ): VNode = {
    val effects = AppRouter.router.map { router =>
      router.transformSource[Action](_ => Observable.empty)
    }
    Store.create(initActions, State(), effects).flatMap(view(_, logger, parent))

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

        val todoModule = TodoModule(loggerSink, store)

        table(
          tbody(
            tr(
              td("Last action: ", child <-- store.map(_.lastAction))
            ),
            tr(
              span("Add item 'show log' to test router as effect.")
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



object AppRouter {

  sealed trait Page {
    def title: String
  }
  object TodoPage extends Page {
    val title = "Todo List"
  }
  case class LogPage(message: String) extends Page {
    val title = "Log page"
  }

  val baseUrl: BaseUrl = BaseUrl.until_# + "#"

  object Router extends outwatch.router.Router[Page]

  val config = Router.Config { builder =>
    import builder._

    builder
      .rules(
        ("log" / remainingPath).caseClass[LogPage] ~> { case LogPage(message) => Logger(Logger.Init("Message: " + message)) },
        "todo".const(TodoPage) ~> TodoComponent(),
        "log".const(Unit) ~> Router.Replace(LogPage("log only"))
      )
      .notFound(Router.Replace(TodoPage))
  }


//  val router = Router.create(config, baseUrl).unsafeRunSync()

  val create = Router.createRef(config, baseUrl)
  lazy val router = Router.get
  lazy val push = router.map(_.mapSink[Page](Router.Push))
  lazy val replace = router.map(_.mapSink[Page](Router.Replace))
  lazy val force = router.map(_.mapSink[AbsUrl](Router.Force))

  def asEffect[E, A](f: PartialFunction[E, Router.Action]) = router.map { router =>
    router.transformPipe[E, A](_.collect(f))(_ => Observable.empty)
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

  import AppRouter._
  import outwatch.dom._

  val updatePageTitle : Option[Page] => Unit = {
    case Some(p) =>
      dom.document.title = p.title
    case None =>
      dom.document.title = "Not found"
  }

  def main(args: Array[String]): Unit = {

    AppRouter.create.flatMap { router =>
      router.map(_.page).subscribe { p =>
        updatePageTitle(p)
        Continue
      }

      Styles.subscribe(_.addToDocument())

      OutWatch.render("#app", BaseLayout(router.map(_.node)))
    }.unsafeRunSync()
  }


}
