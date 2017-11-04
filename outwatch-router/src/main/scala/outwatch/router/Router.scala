package outwatch.router

import monix.execution.Ack.Continue
import monix.execution.Scheduler.Implicits.global
import monix.execution.cancelables.SingleAssignmentCancelable
import monix.execution.{Ack, Cancelable}
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.Unbounded
import org.scalajs.dom
import outwatch.Sink
import outwatch.dom.{Handlers, VNode}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.scalajs.js

/**
  * Created by marius on 26/06/17.
  */

trait Router {

  type Page

  case class Redirect[+Page](page: Page, replace: Boolean = false)

  private val pageHandler = Handlers.createHandler[Redirect[Page]]().unsafeRunSync()

  val set : Sink[Page] = pageHandler.redirectMap(p => Redirect(p))
  val replace : Sink[Page] = pageHandler.redirectMap(p => Redirect(p, replace = true))

  protected type Parsed[Page] = Either[Redirect[Page], Page]

  protected case class Rule[Page](
    parse: Path => Option[Parsed[Page]],
    path: Page => Option[Path],
    target: Page => Option[VNode]
  )

  protected class RouterConfig[Page](rules: Seq[Rule[Page]], notFound: Parsed[Page]) {

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
  }

  protected object RouterConfig {

    case class RouterConfigBuilder[Page](rules: Seq[Rule[Page]]) extends PathParser {

      implicit def toVNodeFunc[P](f: => VNode): P => VNode = _ => f

      implicit class route[P <: Page](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {
        def ~>(f: => P => VNode): Rule[Page] = Rule(
          p => rf.route.parse(p).map(p => Right(p)),
          p => ct.unapply(p).map(rf.route.pathFor),
          p => ct.unapply(p).map(f)
        )
      }

      implicit class toRedirect[P](rf: RouteFragment[P]) {
        def ~>[P2 <: Page](f: => Redirect[P2]): Rule[P2] = Rule(
          p => rf.route.parse(p).map(p => Left(f)),
          p => None,
          p => None
        )
      }

      def rules(r: Rule[Page]*): RouterConfigBuilder[Page] = this.copy(rules = r)

      def notFound(page: Redirect[Page]) = new RouterConfig[Page](rules, Left(page))
    }

    def apply(builder: (RouterConfigBuilder[Page]) => RouterConfig[Page]): RouterConfig[Page] =
      builder(RouterConfigBuilder[Page](Seq.empty))
  }



  protected val config : RouterConfig[Page]

  val baseUrl : BaseUrl

  private def parseUrl(absUrl: AbsUrl): Parsed[Page] = {
    val path = Path(absUrl.value.replaceFirst(baseUrl.value, ""))
    val parsed = config.parse(path)
    parsed
  }

  private def pageToUrl(page: Page): AbsUrl = {
    val path = config.path(page).getOrElse(Path.root)
    path.abs(baseUrl)
  }

  protected def missingRuleFor(page: Page): VNode = {
    outwatch.dom.div(s"Page not found, check the routes config")
  }

  private def pageToNode(page: Page) : VNode = {
    config.target(page).getOrElse(missingRuleFor(page))
  }

  private val parsedToPageWithEffects: Parsed[Page] => Page = {
    case Right(page) =>
      page
    case Left(Redirect(page, true)) =>
      dom.window.history.replaceState("", "", pageToUrl(page).value)
      page
    case Left(Redirect(page, false)) =>
      dom.window.history.pushState("", "", pageToUrl(page).value)
      page
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

  private val popStateObservable = fromEvent(dom.window, "popstate")
    .startWith(Seq(""))
    .map { _ => parseUrl(AbsUrl.fromWindow) }

  val pageChanged: Observable[Page] = Observable.merge(
    popStateObservable,
    pageHandler.map(r => Left(r))
  )
    .map(parsedToPageWithEffects)
    .replay(1)
    .refCount

  private val vnodeSource = pageChanged.map(pageToNode)
  private val defaultBaseLayout: Observable[VNode] => VNode = { vnode =>
    outwatch.dom.div(outwatch.dom.child <-- vnode)
  }

  def apply(baseLayout: Observable[VNode] => VNode = defaultBaseLayout): VNode = baseLayout(vnodeSource)
}
