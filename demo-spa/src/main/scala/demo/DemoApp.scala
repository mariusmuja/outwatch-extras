package demo

import cats.effect.IO
import demo.styles._
import monix.execution.Ack.Continue
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom
import outwatch.Handler
import outwatch.dom._
import outwatch.dom.dsl.input
import outwatch.extras.redux.{StatefulComponent, StatefulEffectsComponent, Store, _}
import outwatch.extras.router.{BaseUrl, RouterOps}
import outwatch.extras.styles.Styles
import outwatch.extras.{<--<, >-->}

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

  type Effect = Console.Effect

  case class State(
    log: Seq[String] = Seq("Log:")
  ) extends ComponentState {

    val evolve = {
      case InitEffect(message) =>
        this -> Console.Effect.Log(message)
      case Init(message) =>
        copy(log :+ message)
      case LogAction(line) =>
        dom.console.log(s"Log >>>> $line")
        copy(log :+ s"$now : $line")
    }
  }

  def view(handler: State <--< Action)(implicit S: Style): VNode = {
    import AppRouter._
    import outwatch.dom.dsl._

    router.flatMap { router =>
      div(
        div(
          input(value <-- handler.map(_.log.lastOption.getOrElse(""))),
          button(
            onClick(Router.Replace(Page.Log("replaced previous page"))) --> router, "Goto"
          )
        ),
        div(
          textArea(S.textfield, S.material,
            child <-- handler.map(_.log.mkString("\n"))
          )
        )
      )
    }
  }

  val consoleEffect: IO[Console.Effect >--> Action] = Console.merge[Action] {
    case Console.Result.Output(str) => LogAction(str)
  }

  def apply(initActions: Action*): VNode = {
    Store.create(initActions, State(), consoleEffect).flatMap(view)
  }

  def withSink(initActions: Action*): IO[(VNode, Sink[Action])] = {
    Store.create(initActions, State(), consoleEffect).map(store => (view(store), store))
  }
}

object TextField extends TextFieldStyle {

  def apply(actions: Sink[String], minLen : Int = 4)(implicit S: Style): VNode = {
    import outwatch.dom.dsl._

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

      val enterdown = onKeyDown.filter(k => k.keyCode == 13)

      div(
        div(S.textfield, S.material,
          label(S.textlabel, "Enter todo"),
          input(S.textinput,
            onInput.value --> inputTodo,
            value <-- inputTodo,
            prop("value") <-- inputTodo,
            enterdown(inputTodo) --> filteredActions,
            enterdown("") --> inputTodoFiltered
          )
        ),
        button(S.button, S.material,
          onClick(inputTodo) --> filteredActions,
          onClick("") --> inputTodoFiltered,
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

  sealed trait Effect
  object Effect{
    case class GoTo(page: Page) extends Effect
  }

  private def newID = Random.nextInt

  case class Todo(id: Int, value: String)

  case class State(todos: Seq[Todo] = Seq.empty) extends ComponentState { self =>
    val evolve = {
      case add @ AddTodo(value) =>
        if (value == "show log") {
          self -> Effect.GoTo(Page.Log("Log as effect"))
        } else
        copy(todos = todos :+ Todo(newID, value))

      case remove @ RemoveTodo(todo) =>
        copy(todos = todos.filter(_.id != todo.id))
    }
  }

  import outwatch.dom.dsl.styles._
  import extra._

  val slideInOut = VDomModifier(
    transition.accum := "transform .2s ease-in-out",
    transform := "translateX(-50px)",
    transform.delayed := "translateX(0px)",
    transform.remove := "translateX(50px)",
  )

  val fadeInOut = VDomModifier(
    transition.accum :=  "opacity .2s ease-in-out",
    opacity := 0,
    opacity.delayed := 1,
    opacity.remove := 0
  )

  val moveInOut =  VDomModifier(slideInOut, fadeInOut)

  private def todoItem(todo: Todo, actions: Sink[Action], stl: Style): VNode = {
    import outwatch.dom.dsl._
    li(
      key := s"${todo.id}", moveInOut,
      span(todo.value),
      button(stl.button, stl.material, onClick(RemoveTodo(todo)) --> actions, "Delete")
    )
  }

  def view(
    store: Action >--> State,
    logger: Sink[Logger.Action],
    parent: Sink[TodoComponent.Action]
  )(implicit S: Style): VNode = {

    import AppRouter._
    import outwatch.dom.dsl._

    val loggerSink = logger.redirectMap[Action]{
      case AddTodo(value) => Logger.LogAction(s"Add $value")
      case RemoveTodo(todo) => Logger.LogAction(s"Remove ${todo.value}")
    }
    val parentSink = parent.redirectMap[Action] {
      case AddTodo(value) => TodoComponent.AddTodo(value)
      case RemoveTodo(todo) => TodoComponent.RemoveTodo(todo.value)
    }

    Handler.create[Action].flatMap { actions =>

      val todoViews = store.map(_.todos.map(todoItem(_, actions, S)))

      AppRouter.push.flatMap { router =>
        div(
          managed(store <-- actions, loggerSink <-- actions, parentSink <-- actions),
          TextField(actions.redirectMap(AddTodo)),
          button(S.button, S.material,
            onClick(Page.Log("from 'Log only'")) --> router, "Log only"
          ),
          ul(children <-- todoViews)
        )
      }
    }

  }

  def apply(logger: Sink[Logger.Action],
    parent: Sink[TodoComponent.Action],
    initActions: Action*
  ): VNode = {
    val routerEffects = AppRouter.asEffect[Effect, Action] {
      case Effect.GoTo(page) => AppRouter.Router.Push(page)
    }
    Store.create(initActions, State(), routerEffects).flatMap(view(_, logger, parent))
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
    import outwatch.dom.dsl._

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

object AppPages {

  sealed abstract class Page(val title: String)

  object Page {

    object Todo extends Page("Todo List")

    case class Log(message: String) extends Page("Log page")

  }

}



object AppRouter extends RouterOps {

  type Page = AppPages.Page
  val Page = AppPages.Page

  val baseUrl: BaseUrl = BaseUrl.until_# + "#"

  val config = Router.Config { builder =>
    import builder._

    builder
      .rules(
        ("log" / remainingPath).caseClass[Page.Log] ~> { case Page.Log(message) => Logger(Logger.Init("Message: " + message)) },
        "todo".const(Page.Todo) ~> TodoComponent(),
        "log".const(Unit) ~> Router.Replace(Page.Log("log only"))
      )
      .notFound(Router.Replace(Page.Todo))
  }

}


object Console extends EffectsHandler {

  sealed trait Effect
  object Effect {
    case class Log(str: String) extends Effect
  }

  sealed trait Result
  object Result {
    case class Output(str: String) extends Result
  }

  val effects: Effect => Observable[Result] = {
    case Effect.Log(str) =>
      dom.console.log(s"In console: $str")
      Observable.pure(Result.Output(str)).delayOnNext(1000.millis)
  }
}


object BaseLayout {

  def apply(node: Observable[VNode]): VNode = {
    import outwatch.dom.dsl._
    div(
      h4("Todo"),
      div(child <-- node)
    )
  }
}


object DemoApp {

  import AppRouter._
  import outwatch.dom.OutWatch

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

      OutWatch.renderInto("#app", BaseLayout(router.map(_.node)))
    }.unsafeRunSync()
  }



}
