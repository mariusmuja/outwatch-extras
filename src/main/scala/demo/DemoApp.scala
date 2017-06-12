package demo

import com.softwaremill.quicklens._
import demo.styles.MdlStyles
import org.scalajs.dom
import org.scalajs.dom.{Event, EventTarget, console}
import outwatch.Sink
import outwatch.dom.Handlers._
import outwatch.dom.{OutWatch, VNode}
import outwatch.extras.StyleAdaptors._
import outwatch.extras._
import rxscalajs.Observable
import rxscalajs.Observable.Creator
import rxscalajs.subscription.AnonymousSubscription

import scala.scalajs.js
import scala.scalajs.js.{Date, JSApp}
import scala.util.Random
import scalacss.DevDefaults._


case class Store[M](model: Observable[M], actions: Sink[Action]) {

  def map[R](project: M => R): Store[R] = Store(model.map(project), actions)
}

object Store {
  import scala.language.implicitConversions
  implicit def toSink[M](store: Store[M]): Sink[Action] = store.actions
  implicit def toObservable[M](store: Store[M]): Observable[M] = store.model
}

trait LogAreaStyle extends ComponentStyle {

  class Style extends StyleSheet.Inline {

    import dsl._

    val textfield = style(
      MdlStyles.textfield,
      height(400.px),
      width(400.px).important,
      fontFamily :=! "Courier New",
      fontSize(14.px).important
    )
  }

  object defaultStyle extends Style with Styles.Publish
}


object Logger extends Component with
                      LogAreaStyle {
  case class LogAction(action: String) extends Action

  case class Model(
    log: Seq[String] = Seq("Log:")
  )

  private def now = (new Date).toLocaleString()

  val reducer: Reducer = {
    case (state, LogAction(line)) =>
      console.log(s"Log $line")
      modify(state)(_.log).using(_ :+ s"$now : $line")
  }

  def view(model: Store[Model], stl: Style = defaultStyle): VNode = {
    import outwatch.dom._

    textarea(stl.textfield,
      child <-- model.map(_.log.mkString("\n"))
    )
  }

  def apply(model: Store[Model], stl: Style = defaultStyle): VNode = view(model, stl)
}


trait TextFieldStyle extends ComponentStyle {

  class Style extends StyleSheet.Inline {

    import dsl._

    val textfield = style (
      MdlStyles.textfield,
      marginRight(8.px).important
    )

    val textinput = style (
      MdlStyles.textinput
    )

    val textlabel = style (
      MdlStyles.textlabel
    )

    val button = style (
      MdlStyles.button
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
      div(stl.textfield,
        label(stl.textlabel, "Enter todo"),
        input(stl.textinput,
          inputString --> inputTodo,
          value <-- inputTodo,
          enterdown(inputTodo) --> actions,
          enterdown("") --> inputTodo
        )
      ),
      button(stl.button,
        click(inputTodo) --> actions,
        click("") --> inputTodo,
        disabled <-- disabledValues,
        "Submit"
      )
    )
  }

}


trait TodoModuleStyle extends ComponentStyle {

  class Style extends StyleSheet.Inline {

    import dsl._

    val textinput = style(
      MdlStyles.textinput
    )

    val textlabel = style(
      MdlStyles.textlabel
    )

    val button = style(
      MdlStyles.button,
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
  case class Model(todos: Seq[Todo] = Seq.empty)

  private def newID = Random.nextInt

  val reducer: Reducer = {
    case (state, RemoveTodo(todo)) =>
      modify(state)(_.todos).using(_.filter(_.id != todo.id))
    case (state, AddTodo(value)) =>
      modify(state)(_.todos).using(_ :+ Todo(newID, value))
  }

  // simulate some async effects by logging actions with a delay
  override val effects: Effects.Handler = {
    case AddTodo(s) =>
      Observable.interval(100).take(1)
        .mapTo(LogAction(s"Add action: $s"))
    case RemoveTodo(todo) =>
      Observable.interval(100).take(1)
        .mapTo(LogAction(s"Remove action: ${todo.value}"))
  }


  private def todoItem(todo: Todo, actions: Sink[Action], stl: Style): VNode = {
    import outwatch.dom._

    li(
      span(todo.value),
      button(stl.button, click(RemoveTodo(todo)) --> actions, "Delete")
    )
  }

  def apply(store: Store[Model], stl: Style = defaultStyle): VNode = {
    import outwatch.dom._

    val stringSink = store.redirect[String] { item => item.map(AddTodo) }

    val todoViews = store.map(_.todos.map(todoItem(_, store.actions, stl)))

    div(
      TextField(stringSink),
      button(stl.button,
        click(Router.LogPage) --> store,
        "Log only"
      ),
      ul(children <-- todoViews)
    )
  }

}

object TodoComponent extends EffectsComponent {
  import TodoModule.{AddTodo, RemoveTodo}

  case class Model(
    lastAction: String = "None",
    todo: TodoModule.Model = TodoModule.Model(),
    log: Logger.Model = Logger.Model()
  )

  private val lastActionReducer: Reducer = {
    case (model, AddTodo(_)) => model.modify(_.lastAction).setTo("Add")
    case (model, RemoveTodo(_)) => model.modify(_.lastAction).setTo("Remove")
  }

  val reducer: Reducer = combineReducers(
    lastActionReducer,
    subReducer(TodoModule.reducer, modify(_)(_.todo)),
    subReducer(Logger.reducer, modify(_)(_.log))
  )

  override val effects: Effects.Handler = combineEffects(
    TodoModule.effects
  )

  def apply(store: Store[Model]): VNode = {
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



object Router {

  trait Page extends Action {
    def unapply(path: Path): Boolean = if (path.url == "/todo") true else false
  }

  case class Path(url: String)

  private val actions = createHandler[Action]()
  private var effectsSub : Option[AnonymousSubscription] = None


  object TodoPage extends Page

  object LogPage extends Page

  def createNode[M](initialState: => M,
    reducer: Component.ReducerFull[M],
    create: Store[M] => VNode,
    effects: Effects.HandlerFull = Effects.noEffects): VNode = {

    val initState = initialState
    val model = actions
      .scan(initState)(reducer)
      .startWith(initState)
      .publishReplay(1)
      .refCount
    effectsSub.foreach(_.unsubscribe())
    effectsSub = Option(actions <-- actions.flatMap(a => effects(a).merge(pageChange(a))))

    create(Store(model, actions))
  }

  def createNode(component: EffectsComponent)(
    initialState: component.Model,
    creator: Store[component.Model] => VNode
  ): VNode = {
    createNode(initialState, component.reducerFull, creator, component.effectsFull)
  }

  def createNode(component: Component)(
    initialState: component.Model,
    creator: Store[component.Model] => VNode
  ): VNode = {
    createNode(initialState, component.reducerFull, creator)
  }


  def pageChange: Effects.Handler = {
    case TodoPage =>
      dom.document.location.href = (dom.document.location.href + "#todo")
      Observable.empty
    case LogPage =>
      dom.document.location.href = (dom.document.location.href + "#log")
      Observable.empty
    case _ =>
      Observable.empty
  }

  def pageToNode(page: Page) : VNode = {
    page match {
      case TodoPage =>
        createNode(TodoComponent)(TodoComponent.Model(), TodoComponent(_))

      case LogPage =>
        createNode(Logger)(Logger.Model(), Logger(_))
    }
  }

  def eventListener(target: EventTarget, event: String): Observable[Event] =
    Observable.create { subscriber =>
      val eventHandler: js.Function1[Event, Unit] = (e: Event) => subscriber.next(e)
      target.addEventListener(event, eventHandler)
      val cancel: Creator = () => {
        target.removeEventListener(event, eventHandler)
        subscriber.complete()
      }
      cancel
    }


  val location = eventListener(dom.window, "popstate").map(_ =>
    dom.document.location.href
  ).startWith(dom.document.location.href)

  val pages = location.map { href =>
    if (href.endsWith("todo")) TodoPage
    else LogPage
  }


  def apply(): VNode = {
    import outwatch.dom._



    val view = pages.map(pageToNode)

    div(child <-- view)
  }

}




object DemoApp extends JSApp {

  def main(): Unit = {
    Styles.subscribe(_.addToDocument())
    OutWatch.render("#app", Router())
  }
}
