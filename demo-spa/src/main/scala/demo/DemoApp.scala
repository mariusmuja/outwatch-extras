package demo

import demo.styles._
import org.scalajs.dom
import org.scalajs.dom.{Event, EventTarget, console}
import outwatch.Sink
import outwatch.dom.{Handlers, VNode}
import outwatch.extras._
import outwatch.extras.router.{Path, PathParser}
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



object Logger extends StyledComponent with
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

  def view(store: Store[State, Action])(implicit stl: Style): VNode = {
    import outwatch.dom._

    div(
      div(
        input(value <-- store.map(_.log.lastOption.getOrElse(""))),
        button(
          click(Router.LogPage(1)) --> store, "Goto"
        )
      ),
      div(
        textarea(stl.textfield, stl.material,
          child <-- store.map(_.log.mkString("\n"))
        )
      )
    )
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


object TodoModule extends StyledComponentWithEffects with
                          TodoModuleStyle {

  import Logger.LogAction

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

    // simulate some async effects by logging actions with a delay
    override def effects = {
      case AddTodo(s) =>
        Observable.interval(500.millis).take(1)
          .mapTo(LogAction(s"Add ${if (todos.isEmpty) "first " else ""}action: $s"))
      case RemoveTodo(todo) =>
        Observable.interval(500.millis).take(1)
          .mapTo(LogAction(s"Remove action: ${todo.value}"))
    }
  }

  private def todoItem(todo: Todo, actions: Sink[Action], stl: Style): VNode = {
    import outwatch.dom._
    li(
      key := s"${todo.id}",
      span(todo.value),
      button(stl.button, stl.material, click(RemoveTodo(todo)) --> actions, "Delete")
    )
  }

  def init = State()

  def view(store: Store[State, Action])(implicit stl: Style): VNode = {
    import outwatch.dom._

    val stringSink = store.sink.redirect[String] { item => item.map(AddTodo) }

    val todoViews = store.distinct.map(_.todos.map(todoItem(_, store, stl)))

    div(
      TextField(stringSink),
      button(stl.button, stl.material,
        click(Router.LogPage(10)) --> store, "Log only"
      ),
      ul(children <-- todoViews)
    )
  }
}

object TodoComponent extends Component {
  import TodoModule.{AddTodo, RemoveTodo}

  case class State(
    lastAction: String = "None"
  ) extends ComponentState {

    def evolve = {
      case AddTodo(value) => copy(lastAction = s"Add $value")
      case RemoveTodo(todo) => copy(lastAction = s"Remove ${todo.value }")
    }
  }

  def init = State()

  def view(store: Store[State, Action]): VNode = {
    import outwatch.dom._

    table(
      tbody(
        tr(
          td("Last action: ", child <-- store.map(_.lastAction))
        ),
        tr(
          td(TodoModule(store))
        ),
        tr(
          td(Logger(store))
        )
      )
    )
  }
}



object Router {

  private val actionsBase = Handlers.createHandler[Action]()
  private val actions = Handler(actionsBase).redirect(obs => obs merge obs.flatMap(pageChange))

  trait Page extends Action
  object TodoPage extends Page
  case class LogPage(last: Int) extends Page


  sealed trait Redirect[P]

  object Redirect {
    sealed trait Method

    /** The current URL will not be recorded in history. User can't hit ''Back'' button to reach it. */
    case object Replace extends Method

    /** The current URL will be recorded in history. User can hit ''Back'' button to reach it. */
    case object Push extends Method
  }

  final case class RedirectToPage[P](page: P, method: Redirect.Method)

  final case class RedirectToPath[P](path: Path, method: Redirect.Method)

  type Parsed[Page] = Either[Redirect[Page], Page]

  final case class Rule[Page, Target](
    parse: Path => Option[Parsed[Page]],
    path: Page => Option[Path],
    target: Page => Option[Target]
  )

  class RouterConfig[Page, Target](
    rules: Seq[Rule[Page, Target]],
    val notFound: Parsed[Page]
  ) {

    private def findFirst[T, R](list: List[T => Option[R]])(arg: T): Option[R] = {
      list match {
        case Nil => None
        case head :: tail =>
          val res = head(arg)
          if (res.isDefined) res else findFirst(tail)(arg)
      }
    }

    def parse(path: Path): Option[Parsed[Page]] = findFirst(rules.map(_.parse).toList)(path)

    def path(page: Page): Option[Path] = findFirst(rules.map(_.path).toList)(page)

    def target(page: Page): Option[Target] = findFirst(rules.map(_.target).toList)(page)
  }

  object RouterConfig {

    case class RouterConfigBuilder[Page, Target](
      rules: Seq[Rule[Page, Target]]
    ) extends PathParser {

      implicit def toFunc[P](f: => Target): P => Target = _ => f

      implicit def redirectToParsed[P](r: Redirect[P]): Parsed[P] = Left(r)
      implicit def pageToParsed[P](p: P): Parsed[P] = Right(p)


      implicit class route[P <: Page](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {

        val route = rf.route

        def ~>(f: => P => Target) : Rule[Page, Target] = Rule(
          p => route.parse(p).map(p => p),
          p => ct.unapply(p).map(route.pathFor),
          p => ct.unapply(p).map(f)
        )
      }


      def rules(r: Rule[Page, Target]*): RouterConfigBuilder[Page, Target] = this.copy(rules = r)

      def notFound(page: Parsed[Page]) = new RouterConfig[Page, Target](rules, page)
    }

    def apply[Page, Target] (builder: RouterConfigBuilder[Page, Target] => RouterConfig[Page, Target]) =
      builder(RouterConfigBuilder[Page, Target](Seq.empty))
  }


  val config = RouterConfig[Page, VNode] { cfg =>
    import cfg._

    cfg.rules(
      ("log" / int).caseClass[LogPage] ~> { case LogPage(p) => Logger(actions, Logger.Init("Init logger: " + p)) },
      "todo".const(TodoPage) ~> TodoComponent(actions)
    )
      .notFound(Right(TodoPage))
  }





  def pathToPage(path: Path): Page = {
    val rest = path.map { str =>
      val index = str.indexOf("#")
      str.substring(index+1)
    }
    config.parse(rest).getOrElse(
      config.notFound
    ).right.get
  }

  def pageToPath(page: Page): Path = {
    val path = config.path(page).getOrElse(Path(""))

    val str = dom.document.location.href
    val index = str.indexOf("#")
    val prefix = str.substring(0, index + 1)

    val result = Path(prefix) + path
    console.log(""+result.value)
    result
  }

  def pageToNode(page: Page) : VNode = {
    config.target(page).get
  }


  private def pageChange[S]: Action => Observable[Action] = {
    case p: Page =>
      dom.window.history.pushState("", "", pageToPath(p).value)
      Observable.empty
    case _ =>
      Observable.empty
  }

  private def fromEvent(target: EventTarget, event: String): Observable[Event] =
    Observable.create { subscriber =>
      val eventHandler: js.Function1[Event, Unit] = (e: Event) => subscriber.next(e)
      target.addEventListener(event, eventHandler)
      val cancel: Creator = () => {
        target.removeEventListener(event, eventHandler)
        subscriber.complete()
      }
      cancel
    }


  val popStatePages = fromEvent(dom.window, "popstate")
    .startWith(dom.document.createEvent("PopStateEvent"))
    .map { _ => Path(dom.document.location.href) }
    .map(pathToPage)

  val actionPages = actions.collect { case e: Page => e }

  val nodes = popStatePages.merge(actionPages)
      .map(pageToNode)


  val view : (Handler[Action], Observable[VNode]) => VNode = (_, nodes) => outwatch.dom.div(outwatch.dom.child <-- nodes)

  def render(view: (Handler[Action], Observable[VNode]) => VNode) : VNode = view(actions, nodes)

  def apply(): VNode = {
    render(view)
  }

}


object DemoApp extends JSApp {

  import outwatch.dom._

  def main(): Unit = {
    Styles.subscribe(_.addToDocument())
    OutWatch.render("#app", Router())
  }
}
