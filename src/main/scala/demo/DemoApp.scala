package demo

import demo.styles._
import org.scalajs.dom
import org.scalajs.dom.{Event, EventTarget, console}
import outwatch.Sink
import outwatch.dom.{Handlers, OutWatch, VNode}
import outwatch.extras._
import outwatch.extras.router.{Path, PathParser}
import outwatch.styles.Styles
import rxscalajs.Observable
import rxscalajs.Observable.Creator
import rxscalajs.subscription.AnonymousSubscription

import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.{Date, JSApp}
import scala.util.Random
import scalacss.DevDefaults._



object Logger extends Component with
                      LogAreaStyle {
  case class LogAction(action: String) extends Action

  private def now = (new Date).toLocaleString()

  case class State(
    log: Seq[String] = Seq("Log:")
  ) extends EvolvableState {
    def evolve = {
      case LogAction(line) =>
        console.log(s"Log $line")
        copy(log :+ s"$now : $line")
    }
  }

  override val effects: EffectsHandler = {
    case (_, TodoModule.AddTodo(s)) =>
      console.log("Add todo effect")
      Observable.empty
  }


  def view(store: Store[State, Action],  stl: Style = defaultStyle): VNode = {
    import outwatch.dom._

    textarea(stl.textfield, stl.material,
      child <-- store.map(_.log.mkString("\n"))
    )
  }


  def apply(handler: Handler[Action], stl: Style = defaultStyle): VNode = {
    val store = Store(handler, State(), reducerFull).share
    view(store)
  }
}

object TextField extends TextFieldStyle {

  def apply(actions: Sink[String], minLen : Int = 4, stl: Style = defaultStyle): VNode = {
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

  import Logger.LogAction

  case class AddTodo(value: String) extends Action
  case class RemoveTodo(todo: Todo) extends Action


  private def newID = Random.nextInt


  case class Todo(id: Int, value: String)

  case class State(todos: Seq[Todo] = Seq.empty) extends EvolvableState {
    def evolve = {
      case AddTodo(value) =>
        copy(todos = todos :+ Todo(newID, value))
      case RemoveTodo(todo) =>
        copy(todos = todos.filter(_.id != todo.id))
    }
  }

  // simulate some async effects by logging actions with a delay
  override val effects: EffectsHandler = {
    case (state, AddTodo(s)) =>
      Observable.interval(2000).take(1)
        .mapTo(LogAction(s"Add ${if (state.todos.isEmpty) "first " else ""}action: $s"))
    case (_, RemoveTodo(todo)) =>
      Observable.interval(2000).take(1)
        .mapTo(LogAction(s"Remove action: ${todo.value}"))
  }


  private def todoItem(todo: Todo, actions: Sink[Action], stl: Style): VNode = {
    import outwatch.dom._
    li(
      span(todo.value),
      button(stl.button, stl.material, click(RemoveTodo(todo)) --> actions, "Delete")
    )
  }

  def view(store: Store[State, Action], stl: Style = defaultStyle): VNode = {
    import outwatch.dom._

    val stringSink = store.redirect[String] { item => item.map(AddTodo) }

    val todoViews = store.map(_.todos.map(todoItem(_, store, stl)))

    div(
      TextField(stringSink),
      button(stl.button, stl.material,
        click(Router.LogPage(10)) --> store, "Log only"
      ),
      ul(children <-- todoViews)
    )
  }

  def apply(handler: Handler[Action]): VNode = {
    val store = Store(handler, State(), reducerFull).share
    view(store)
  }
}

object TodoComponent extends Component with NoEffects {
  import TodoModule.{AddTodo, RemoveTodo}

  case class State(
    lastAction: String = "None"
  ) extends EvolvableState {
    def evolve = {
      case AddTodo(value) => copy(lastAction = s"Add $value")
      case RemoveTodo(todo) => copy(lastAction = s"Remove ${todo.value }")
    }
  }

  def view(store: Store[State, Action]): VNode = {
    import outwatch.dom._

    table(
      tbody(
        tr(
          td("Last action: ", child <-- store.map(_.lastAction))
        ),
        tr(
          td(TodoModule(store.handler))
        ),
        tr(
          td(Logger(store.handler))
        )
      )
    )
  }

  def apply(handler: Handler[Action]): VNode = {
    val store = Store(handler, State(), reducerFull).share
    view(store)
  }

}



object Router {

  private val actionSink = Handlers.createHandler[Action]()
  private var effectsSub : Option[AnonymousSubscription] = None

  private def createNode[State](
    initialState: => State,
    reducer: Component.ReducerFull[State],
    view: Store[State, Action] => VNode,
    effects: Effects.HandlerFull[State]
  ): VNode = {

    val effectsWithPageChange: Effects.HandlerFull[State] = (s,a) => effects(s,a) merge pageChange(s,a)
//
//    val initState = initialState
//    val source = actionSink
//      .scan(initState)(reducer)
//      .startWith(initState)
//      .publishReplay(1)
//      .refCount
//
//    effectsSub.foreach(_.unsubscribe())
//    effectsSub = Option(
//      actionSink <-- actionSink.withLatestFrom(source).flatMap{ case (a,s) => effectsWithPageChange(s,a)
//      }
//    )
//    view(Store(source, actionSink))


    val initStateAndEffects = (initialState, Observable.just[Action]())
    val source = actionSink
      .scan(initStateAndEffects) { case ((s, _), a) =>
        (reducer(s, a), effectsWithPageChange(s, a))
      }
      .startWith(initStateAndEffects)
      .publishReplay(1)
      .refCount

    effectsSub.foreach(_.unsubscribe())
    effectsSub = Option(actionSink <-- source.flatMap(_._2))
    view(Store(source.map(_._1), actionSink))
  }

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

    private def findFirst[T,R](list: List[T => Option[R]])(arg: T): Option[R] = {
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

      implicit class route[P <: Page](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {

        val route = rf.route

        def ~>(f: P => Target) : Rule[Page, Target] = Rule(
          p => route.parse(p).map(Right(_)),
          p => ct.unapply(p).map(route.pathFor),
          p => ct.unapply(p).map(f)
        )

        def ~>(f: => Target) : Rule[Page, Target] = Rule(
          p => route.parse(p).map(Right(_)),
          p => ct.unapply(p).map(route.pathFor),
          p => ct.unapply(p).map(p => f)
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
      ("log" / int).xmap(LogPage)(LogPage.unapply(_).head) ~> (p => Logger(actionSink)),
      "todo".const(TodoPage) ~> TodoComponent(actionSink)
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


  private def pageChange[S]: Effects.HandlerFull[S] = { (_, action) =>
    action match {
      case p: Page =>
        dom.window.history.pushState("", "", pageToPath(p).value)
        Observable.empty
      case _ =>
        Observable.empty
    }
  }

  private def eventListener(target: EventTarget, event: String): Observable[Event] =
    Observable.create { subscriber =>
      val eventHandler: js.Function1[Event, Unit] = (e: Event) => subscriber.next(e)
      target.addEventListener(event, eventHandler)
      val cancel: Creator = () => {
        target.removeEventListener(event, eventHandler)
        subscriber.complete()
      }
      cancel
    }


  val location = eventListener(dom.window, "popstate")
    .map(_ => Path(dom.document.location.href))
    .startWith(Path(dom.document.location.href))

  val pages = location.map(pathToPage) merge actionSink.collect { case e: Page => e }

  def apply(): VNode = {
    import outwatch.dom._
    div(child <-- pages.map(pageToNode))
  }

}




object DemoApp extends JSApp {

  def main(): Unit = {
    Styles.subscribe(_.addToDocument())
    OutWatch.render("#app", Router())
  }
}
