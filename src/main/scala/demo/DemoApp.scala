package demo

import com.softwaremill.quicklens._
import demo.styles.MdlStyles
import org.scalajs.dom
import org.scalajs.dom.{Event, EventTarget, console}
import outwatch.Sink
import outwatch.dom.Handlers
import outwatch.dom.{OutWatch, VNode}
import outwatch.extras._
import outwatch.styles.{ComponentStyle, Styles}
import rxscalajs.Observable
import rxscalajs.Observable.Creator
import rxscalajs.subscription.AnonymousSubscription

import scala.scalajs.js
import scala.scalajs.js.{Date, JSApp}
import scala.util.Random
import scalacss.DevDefaults._


trait LogAreaStyle extends ComponentStyle {

  class Style extends StyleSheet.Inline with MdlStyles {

    import dsl._

    val textfield = style(
      mdl.textfield,
      height(400.px),
      width(400.px).important,
      fontFamily :=! "Courier New",
      fontSize(14.px).important
    )
  }

  object defaultStyle extends Style with Styles.Publish
}


object Logger extends EffectsComponent with
                      LogAreaStyle {
  case class LogAction(action: String) extends Action

  case class State(
    log: Seq[String] = Seq("Log:")
  )

  private def now = (new Date).toLocaleString()

  val reducer: Reducer = {
    case (state, LogAction(line)) =>
      console.log(s"Log $line")
      modify(state)(_.log).using(_ :+ s"$now : $line")
  }

  override val effects: EffectsHandler = {
    case (_, TodoModule.AddTodo(s)) =>
      console.log("Add todo effect")
      Observable.empty
  }

  def apply(store: Store[State, Action], stl: Style = defaultStyle): VNode = {
    import outwatch.dom._

    textarea(stl.textfield, stl.material,
      child <-- store.map(_.log.mkString("\n"))
    )
  }
}


trait TextFieldStyle extends ComponentStyle {

  class Style extends StyleSheet.Inline with MdlStyles {

    import dsl._

    val textfield = style (
      mdl.textfield,
      marginRight(8.px).important
    )

    val textinput = style (
      mdl.textinput
    )

    val textlabel = style (
      mdl.textlabel
    )

    val button = style (
      mdl.button
    )
  }

  object defaultStyle extends Style with Styles.Publish
}

object TextField extends TextFieldStyle {

  def apply(actions: Sink[String], stl: Style = defaultStyle): VNode = {
    import outwatch.dom._

    val inputTodo = createStringHandler()

    val disabledValues = inputTodo
      .map(_.length < 4)
      .startWith(true)

    val enterdown = keydown.filter(_.keyCode == 13)

    div(
      div(stl.textfield, stl.material,
        label(stl.textlabel, "Enter todo"),
        input(stl.textinput,
          inputString --> inputTodo,
          value <-- inputTodo,
          enterdown(inputTodo) --> actions,
          enterdown("") --> inputTodo
        )
      ),
      button(stl.button, stl.material,
        click(inputTodo) --> actions,
        click("") --> inputTodo,
        disabled <-- disabledValues,
        "Submit"
      )
    )
  }

}


trait TodoModuleStyle extends ComponentStyle {

  class Style extends StyleSheet.Inline with MdlStyles {

    import dsl._

    val textinput = style(
      mdl.textinput
    )

    val textlabel = style(
      mdl.textlabel
    )

    val button = style(
      mdl.button,
      marginLeft(8.px)
    )
  }

  object defaultStyle extends Style with Styles.Publish
}


object TodoModule extends Component with
                          Effects with
                          TodoModuleStyle {

  import Logger.LogAction

  case class AddTodo(value: String) extends Action
  case class RemoveTodo(todo: Todo) extends Action

  case class Todo(id: Int, value: String)
  case class State(todos: Seq[Todo] = Seq.empty)

  private def newID = Random.nextInt

  val reducer: Reducer = {
    case (state, AddTodo(value)) =>
      modify(state)(_.todos).using(_ :+ Todo(newID, value))
    case (state, RemoveTodo(todo)) =>
      modify(state)(_.todos).using(_.filter(_.id != todo.id))
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

  def apply(store: Store[State, Action], stl: Style = defaultStyle): VNode = {
    import outwatch.dom._

    val stringSink = store.redirect[String] { item => item.map(AddTodo) }

    val todoViews = store.map(_.todos.map(todoItem(_, store, stl)))

    div(
      TextField(stringSink),
      button(stl.button, stl.material,
        click(Router.LogPage) --> store,
        "Log only"
      ),
      ul(children <-- todoViews)
    )
  }

}

object TodoComponent extends EffectsComponent {
  import TodoModule.{AddTodo, RemoveTodo}

  case class State(
    lastAction: String = "None",
    todo: TodoModule.State = TodoModule.State(),
    log: Logger.State = Logger.State()
  )

  private val lastActionReducer: Reducer = {
    case (state, AddTodo(_)) => state.modify(_.lastAction).setTo("Add")
    case (state, RemoveTodo(_)) => state.modify(_.lastAction).setTo("Remove")
  }

  val reducer: Reducer = combineReducers(
    lastActionReducer,
    subReducer(TodoModule.reducer, modify(_)(_.todo)),
    subReducer(Logger.reducer, modify(_)(_.log))
  )

  override val effects: EffectsHandler = {
    combineEffects(
      subEffectHandler(TodoModule.effects, _.todo),
      subEffectHandler(Logger.effects, _.log)
    )
  }

  def apply(store: Store[State, Action]): VNode = {
    import outwatch.dom._

    table(
      tbody(
        tr(
          td("Last action: ", child <-- store.map(_.lastAction))
        ),
        tr(
          td(TodoModule(store.map(_.todo)))
        ),
        tr(
          td(Logger(store.map(_.log)))
        )
      )
    )
  }
}

case class Path(url: String)

case class Rule[M](
  parse: Path => Option[M],
  create: M => VNode
)

case class PathParser(
  rules: Seq[Rule[_]]
)


object Router {

  trait Page extends Action

  private val actionSink = Handlers.createHandler[Action]()
  private var effectsSub : Option[AnonymousSubscription] = None

  object TodoPage extends Page

  object LogPage extends Page

  private def createNode[State](
    initialState: => State,
    reducer: Component.ReducerFull[State],
    creator: Store[State, Action] => VNode,
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
//    creator(Store(source, actionSink))


    val initStateAndEffects = (initialState, Observable.just[Action]())
    val source = actionSink
      .scan(initStateAndEffects) { case ((s, _), a) =>
        reducer(s, a) -> effectsWithPageChange(s, a)
      }
      .startWith(initStateAndEffects)
      .publishReplay(1)
      .refCount

    effectsSub.foreach(_.unsubscribe())
    effectsSub = Option(actionSink <-- source.flatMap(_._2))
    creator(Store(source.map(_._1), actionSink))
  }

  def createNode(component: EffectsComponent)(
    initialState: component.State,
    creator: Store[component.State, Action] => VNode
  ): VNode = {
    createNode(initialState, component.reducerFull, creator, component.effectsFull)
  }
//
//  trait NodeCreator[C <: Component] {
//    val component: C
//    val initialState: component.State
//
//    def create(store: Store[component.State, Action]): VNode
//  }

  def createNode(component: Component)(
    initialState: component.State,
    creator: Store[component.State, Action] => VNode
  ): VNode = {
    createNode[component.State](initialState, component.reducerFull, creator, Effects.noEffects)
  }


  def pathToPage(path: Path): Page = {
    if (path.url.endsWith("log")) LogPage
    else TodoPage
  }

  def pageToPath(page: Page): Path = {
    page match {
      case TodoPage =>
        Path("#todo")
      case LogPage =>
        Path("#log")
    }
  }

  def pageToNode(page: Page) : VNode = {
    page match {
      case TodoPage =>
        createNode(TodoComponent)(TodoComponent.State(), TodoComponent(_))
      case LogPage =>
        createNode(Logger)(Logger.State(), Logger(_))
    }
  }


  private def pageChange[S]: Effects.HandlerFull[S] = { (_, action) =>
    action match {
      case p: Page =>
        dom.window.history.pushState("", "", pageToPath(p).url)
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

  val pages = location.map(pathToPage).merge(actionSink.collect { case e: Page => e })

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
