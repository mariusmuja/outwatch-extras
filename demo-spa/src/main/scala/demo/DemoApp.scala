package demo

import demo.styles._
import org.scalajs.dom
import org.scalajs.dom.{Event, EventTarget, console}
import outwatch.{Sink, SinkUtil}
import outwatch.dom.{Handlers, VNode}
import outwatch.extras._
import outwatch.extras.router.{AbsUrl, BaseUrl, Path, PathParser}
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
          click(Router.LogPage(1).replace) --> router, "Goto"
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



trait Router {

  trait Page
  object Page {
    case class Replace(page: Page) extends Page
    implicit class toReplace(p: Page) {
      def replace = Replace(p)
    }
  }

  type PageSink = Sink[Page]

  protected val routerActions = Handlers.createHandler[Page]()


  case class Redirect[P](page: P)

  protected class RouterConfig[Page](
    rules: Seq[RouterConfig.Rule[Page]],
    val notFound: RouterConfig.Parsed[Page]
  ) {

    private def findFirst[T, R](list: List[T => Option[R]])(arg: T): Option[R] = {
      list match {
        case Nil => None
        case head :: tail =>
          val res = head(arg)
          if (res.isDefined) res else findFirst(tail)(arg)
      }
    }

    def parse(path: Path): Option[RouterConfig.Parsed[Page]] = findFirst(rules.map(_.parse).toList)(path)

    def path(page: Page): Option[Path] = findFirst(rules.map(_.path).toList)(page)

    def target(page: Page): Option[VNode] = findFirst(rules.map(_.target).toList)(page)
  }

  protected object RouterConfig {

    type Parsed[Page] = Either[Redirect[Page], Page]

    final case class Rule[Page](
      parse: Path => Option[Parsed[Page]],
      path: Page => Option[Path],
      target: Page => Option[VNode]
    )

    case class RouterConfigBuilder[Page](
      rules: Seq[Rule[Page]]
    ) extends PathParser {

      implicit def toFunc[P](f: => VNode): P => VNode = _ => f
      implicit def toPage[P, P2 <: Page](f: => P2): P => P2 = _ => f

      implicit class route[P <: Page](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {

        def ~>(f: => P => VNode) : Rule[Page] = Rule(
          p => rf.route.parse(p).map(p => Right(p)),
          p => ct.unapply(p).map(rf.route.pathFor),
          p => ct.unapply(p).map(f)
        )
      }

      implicit class redirect[P](rf: RouteFragment[P]) {
        def ~>[P2 <: Page](f: => P => P2) : Rule[Page] = Rule(
          p => rf.route.parse(p).map(p => Left(Redirect(f(p)))),
          p => None,
          p => None
        )
      }

      def rules(r: Rule[Page]*): RouterConfigBuilder[Page] = this.copy(rules = r)

      def notFound(page: Page) = new RouterConfig[Page](rules, Left(Redirect(page)))
    }

    def apply(builder: (RouterConfigBuilder[Page]) => RouterConfig[Page]): RouterConfig[Page] =
      builder(RouterConfigBuilder[Page](Seq.empty))
  }



  val config : RouterConfig[Page]

  val baseUrl : BaseUrl


  private def urlToPage(absUrl: AbsUrl): Page = {
    val path = Path(absUrl.value.replaceFirst(baseUrl.value, ""))
    val parsed = config.parse(path).getOrElse(
      config.notFound
    )

    parsed match {
      case Right(page) => page
      case Left(Redirect(Page.Replace(page))) =>
        dom.window.history.replaceState("", "", pageToUrl(page).value)
        page
      case Left(Redirect(page)) =>
        dom.window.history.pushState("", "", pageToUrl(page).value)
        page
    }
  }

  private def pageToUrl(page: Page): AbsUrl = {
    val path = config.path(page).getOrElse(Path.root)
    path.abs(baseUrl)
  }

  private def pageToNode(page: Page) : VNode = {
    config.target(page).get
  }

  private def pageChanged[S](p: Page) : Page = {
    p match {
      case Page.Replace(page) =>
        dom.window.history.replaceState("", "", pageToUrl(p).value)
        page
      case page: Page =>
        dom.window.history.pushState ("", "", pageToUrl (p).value)
        page
    }
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

  private val popStateObservable = fromEvent(dom.window, "popstate")
    .startWith(dom.document.createEvent("PopStateEvent"))
    .map { _ => AbsUrl.fromWindow }
    .map(urlToPage)

  private val nodes = popStateObservable.merge(routerActions.map(pageChanged))
      .map(pageToNode)

  def baseLayout(node: Observable[VNode]) : VNode = {
    import outwatch.dom._
    div(outwatch.dom.child <-- node)
  }

  def apply(): VNode = baseLayout(nodes)
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
      "log".const(Unit) ~> LogPage(11).replace
    )
      .notFound(TodoPage.replace)
  }
}


object DemoApp extends JSApp {

  import outwatch.dom.OutWatch

  def main(): Unit = {
    Styles.subscribe(_.addToDocument())

    OutWatch.render("#app", Router())
  }
}
