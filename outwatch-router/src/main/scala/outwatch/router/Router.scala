package outwatch.router

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Scheduler.Implicits.global
import monix.execution.cancelables.SingleAssignmentCancelable
import monix.execution.{Ack, Cancelable}
import monix.reactive.OverflowStrategy.Unbounded
import org.scalajs.dom
import outwatch.dom.{Handlers, Observable, VNode}
import outwatch.extras.{>-->, Pipe}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.scalajs.js

/**
  * Created by marius on 26/06/17.
  */

object Router {

  def set[Page](page: Page) = Action(page)
  def replace[Page](page: Page) = Action(page, replace = true)

  case class Action[+Page](page: Page, replace: Boolean = false)
  case class State[+Page](page: Option[Page], node: VNode)

  private type Parsed[+Page] = Either[Action[Page], Page]

  private[Router] class Config[Page] private (rules: Seq[Config.Rule[Page]], val notFound: Either[Action[Page], VNode]) {

    @tailrec
    private def findFirst[T, R](list: List[T => Option[R]])(arg: T): Option[R] = {
      list match {
        case Nil => None
        case head :: tail =>
          val res = head(arg)
          if (res.isDefined) res else findFirst(tail)(arg)
      }
    }

    private def parse(path: Path): Option[Parsed[Page]] = findFirst(rules.map(_.parse).toList)(path)

    private def path(page: Page): Option[Path] = findFirst(rules.map(_.path).toList)(page)

    private def target(page: Page): Option[VNode] = findFirst(rules.map(_.target).toList)(page)

    def parseUrl(baseUrl: BaseUrl, absUrl: AbsUrl): Option[Parsed[Page]] = {
      val path = Path(absUrl.value.replaceFirst(baseUrl.value, ""))
      val parsed = parse(path)
      parsed
    }

    def pageToUrl(baseUrl: BaseUrl, page: Page): AbsUrl = {
      path(page).getOrElse(Path.root).abs(baseUrl)
    }

    def pageToNode(page: Page) : Option[VNode] = {
      target(page)
    }
  }

  private def invalidConfiguration[P](page: P): (Option[P], VNode) = {
    import outwatch.dom._
    Some(page) -> div(stl("color") := "red", s"Invalid configuration, missing rule for ${page.getClass.getName}")
  }

  object Config {

    private[Config] case class Rule[Page](
      parse: Path => Option[Parsed[Page]],
      path: Page => Option[Path],
      target: Page => Option[VNode]
    )

    private[Config] case class Builder[Page] private(rules: Seq[Rule[Page]]) extends PathParser {

      implicit def toVNodeFunc[P](f: => VNode): P => VNode = _ => f

      implicit class route[P <: Page](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {
        def ~>(f: => P => VNode): Rule[Page] = Rule(
          p => rf.route.parse(p).map(p => Right(p)),
          p => ct.unapply(p).map(rf.route.pathFor),
          p => ct.unapply(p).map(f)
        )
      }

      implicit class toRedirect[P](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {
        def ~>[P2 <: Page](f: => Action[P2]): Rule[P2] = Rule(
          p => rf.route.parse(p).map(p => Left(f)),
          p => ct.unapply(p).map(rf.route.pathFor),
          p => None
        )
      }

      def rules(r: Rule[Page]*): Builder[Page] = copy(rules = r)

      def notFound(page: Action[Page]) = new Config[Page](rules, Left(page))

      def notFound(node: VNode) = new Config[Page](rules, Right(node))
    }

    def apply[Page](builder: Builder[Page] => Config[Page]): Config[Page] =
      builder(Builder[Page](Seq.empty))
  }


  private def fromEvent(target: dom.EventTarget, event: String): Observable[dom.Event] =
    Observable.create(Unbounded) { subscriber =>
      val c = SingleAssignmentCancelable()
      val eventHandler: js.Function1[dom.Event, Ack] = { (e: dom.Event) =>
        subscriber.onNext(e)
        Continue
      }
      target.addEventListener(event, eventHandler)
      c := Cancelable(() => target.removeEventListener(event, eventHandler))
    }


  def create[Page](config: Config[Page], baseUrl: BaseUrl): IO[Action[Page] >--> State[Page]] = {
    Handlers.createHandler[Action[Page]]().map { pageHandler =>
      val parsedToPageWithEffects: Parsed[Page] => Page = {
        case Right(page) =>
          page
        case Left(Action(page, true)) =>
          dom.window.history.replaceState("", "", config.pageToUrl(baseUrl, page).value)
          page
        case Left(Action(page, false)) =>
          dom.window.history.pushState("", "", config.pageToUrl(baseUrl, page).value)
          page
      }

      val popStateObservable = fromEvent(dom.window, "popstate")
        .startWith(Seq(""))
        .map { _ => config.parseUrl(baseUrl, AbsUrl.fromWindow) }

      val pageChanged: Observable[Option[Page]] = Observable.merge(
        popStateObservable,
        pageHandler.map(r => Some(Left(r)))
      )
        .map(_.map(parsedToPageWithEffects))
        .replay(1)
        .refCount

      def pageToNode(page: Page): Option[(Option[Page], VNode)] =
        config.pageToNode(page).map(node => Some(page) -> node)

      val source = pageChanged.map { pageOpt =>

        val (page, node) = pageOpt.flatMap(pageToNode)
          .getOrElse(
            config.notFound.fold(
              action => {
                val page = parsedToPageWithEffects(Left(action))
                pageToNode(page).getOrElse(invalidConfiguration(page))
              },
              node => (None, node)
            )
          )

        State(page, node)
      }

      Pipe(pageHandler, source)
    }
  }
}
