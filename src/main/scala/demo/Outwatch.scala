package demo

import com.softwaremill.quicklens._
import demo.styles.MdlStyles
import outwatch.Sink
import outwatch.dom.{OutWatch, VNode}
import outwatch.extras.StyleAdaptors._
import outwatch.extras._
import outwatch.extras.dom.Handlers._
import rxscalajs.Observable

import scala.scalajs.js.JSApp
import scala.util.Random
import scalacss.DevDefaults._




trait LogAreaStyle extends Style {

  class Style extends StyleSheet.Inline {

    import dsl._

    val textfield = style(
      MdlStyles.textfield,
      height(400.px)
    )
  }

  object defaultStyle extends Style with Styles.Published
}


object LogArea extends Component with
                       LogAreaStyle {
  case class LogAction(action: String) extends Action

  case class Model(
    log: Seq[String] = Seq.empty
  )

  val reducer: Reducer = {
    case (state, LogAction(line)) => modify(state)(_.log).using(_ :+ line)
  }

  def apply(model: Observable[Model], stl: Style = defaultStyle): VNode = {
    import outwatch.extras.dom.all._

    textarea(stl.textfield,
      child <-- model.map(_.log.mkString("\n"))
    )
  }

}


trait TextFieldStyle extends Style {

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

  object defaultStyle extends Style with Styles.Published
}

object TextField extends TextFieldStyle {

  def apply(actions: Sink[String], stl: Style = defaultStyle): VNode = {
    import outwatch.extras.dom.all._

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


trait TodoModuleStyle extends Style {

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

  object defaultStyle extends Style with Styles.Published
}


object TodoModule extends Component with
                          Effects with
                          TodoModuleStyle {

  import LogArea.LogAction

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

  val effects: EffectHandler = {
    case AddTodo(s) =>
      Observable.interval(Random.nextInt(500)).take(1).mapTo(LogAction(s"Add action: $s"))
    case RemoveTodo(todo) =>
      Observable.interval(Random.nextInt(500)).take(1).mapTo(LogAction(s"Remove action: ${todo.value}"))
  }


  private def todoItem(todo: Todo, actions: Sink[Action], stl: Style): VNode = {
    import outwatch.extras.dom.all._
    li(
      span(todo.value),
      button(stl.button, click(RemoveTodo(todo)) --> actions, "Delete")
    )
  }

  def apply(model: Observable[Model], actions: Sink[Action], stl: Style = defaultStyle): VNode = {
    import outwatch.extras.dom.all._

    val stringSink = actions.redirect[String] { item => item.map(AddTodo) }

    val todoViews = model.map(_.todos.map(todoItem(_, actions, stl)))

    div(
      TextField(stringSink),
      ul(children <-- todoViews)
    )
  }

}



object MainComponent extends Component with
                             Effects {
  import TodoModule.{AddTodo, RemoveTodo}

  case class Model(
    lastAction: String = "None",
    todo: TodoModule.Model = TodoModule.Model(),
    log: LogArea.Model = LogArea.Model()
  )

  private val lastActionReducer: Reducer = {
    case (model, AddTodo(_)) => model.modify(_.lastAction).setTo("Add")
    case (model, RemoveTodo(_)) => model.modify(_.lastAction).setTo("Remove")
  }

  val reducer: Reducer = combineReducers(
    lastActionReducer,
    subReducer(TodoModule.reducer, modify(_)(_.todo)),
    subReducer(LogArea.reducer, modify(_)(_.log))
  )

  val effects: EffectHandler = combineEffects(
    TodoModule.effects
  )

  def apply(model: Observable[Model], actions: Sink[Action]): VNode = {
    import outwatch.extras.dom.all._
    table(
      tbody(
        tr(
          td("Last action: ", child <-- model.map(_.lastAction))
        ),
        tr(
          td(TodoModule(model.map(_.todo), actions))
        ),
        tr(
          td(LogArea(model.map(_.log)))
        )
      )
    )
  }
}


object RootModule {

  private val initialState = MainComponent.Model()
  private val actions = createHandler[Action]()
  actions <-- actions.flatMap(MainComponent.effectsFull)

  private val model = actions
    .scan(initialState)(MainComponent.reducer.full)
    .startWith(initialState)
    .share

  val root = MainComponent(model, actions)
}




object DemoApp extends JSApp {

  def main(): Unit = {
    Styles.subscribe(_.addToDocument())
    OutWatch.render("#app", RootModule.root)
  }
}
