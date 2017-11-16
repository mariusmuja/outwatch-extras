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
  case class State[+Page](page: Page, node: VNode)

  private type Parsed[+Page] = Either[Action[Page], Page]

  case class Rule[Page](
    parse: Path => Option[Parsed[Page]],
    path: Page => Option[Path],
    target: Page => Option[VNode]
  )


  private def missingRuleFor[P](page: P): VNode = {
    outwatch.dom.div(s"Page not found, check the routes config")
  }

  class Config[Page] private (rules: Seq[Rule[Page]], notFound: Parsed[Page]) {

    @tailrec
    private def findFirst[T, R](list: List[T => Option[R]])(arg: T): Option[R] = {
      list match {
        case Nil => None
        case head :: tail =>
          val res = head(arg)
          if (res.isDefined) res else findFirst(tail)(arg)
      }
    }

    def parse(path: Path): Parsed[Page] = findFirst(rules.map(_.parse).toList)(path).getOrElse(notFound)

    def path(page: Page): Option[Path] = findFirst(rules.map(_.path).toList)(page)

    def target(page: Page): Option[VNode] = findFirst(rules.map(_.target).toList)(page)

    def parseUrl(baseUrl: BaseUrl, absUrl: AbsUrl): Parsed[Page] = {
      val path = Path(absUrl.value.replaceFirst(baseUrl.value, ""))
      val parsed = parse(path)
      parsed
    }

    def pageToUrl(baseUrl: BaseUrl, page: Page): AbsUrl = {
      path(page).getOrElse(Path.root).abs(baseUrl)
    }

    def pageToNode(page: Page) : VNode = {
      target(page).getOrElse(missingRuleFor(page))
    }
  }

  object Config {

    case class Builder[Page] private(rules: Seq[Rule[Page]]) extends PathParser {

      implicit def toVNodeFunc[P](f: => VNode): P => VNode = _ => f

      implicit class route[P <: Page](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {
        def ~>(f: => P => VNode): Rule[Page] = Rule(
          p => rf.route.parse(p).map(p => Right(p)),
          p => ct.unapply(p).map(rf.route.pathFor),
          p => ct.unapply(p).map(f)
        )
      }

      implicit class toRedirect[P](rf: RouteFragment[P]) {
        def ~>[P2 <: Page](f: => Action[P2]): Rule[P2] = Rule(
          p => rf.route.parse(p).map(p => Left(f)),
          p => None,
          p => None
        )
      }

      def rules(r: Rule[Page]*): Builder[Page] = copy(rules = r)

      def notFound(page: Action[Page]) = new Config[Page](rules, Left(page))
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

      val pageChanged: Observable[Page] = Observable.merge(
        popStateObservable,
        pageHandler.map(r => Left(r))
      )
        .map(parsedToPageWithEffects)
        .replay(1)
        .refCount

      val source = pageChanged.map(p => State(p, config.pageToNode(p)))

      Pipe(pageHandler, source)
    }
  }
}
