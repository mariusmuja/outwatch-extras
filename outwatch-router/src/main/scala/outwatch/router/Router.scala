package outwatch.router

import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import outwatch.Sink
import outwatch.dom.{Handlers, VNode}
import rxscalajs.Observable

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.scalajs.js

/**
  * Created by marius on 26/06/17.
  */

trait Router {

  type Page

  case class Redirect[+Page](page: Page, replace: Boolean = false)

  private val pageHandler = Handlers.createHandler[Redirect[Page]]()

  val sink : Sink[Page] = pageHandler.redirectMap(p => Redirect(p))
  val redirectSink : Sink[Redirect[Page]] = pageHandler

  def replace(page: Page) : Sink[MouseEvent] = pageHandler.redirectMap(_ => Redirect(page, replace = true))

  def set(page: Page) : Sink[MouseEvent] = pageHandler.redirectMap(_ => Redirect(page))

  protected type Parsed[Page] = Either[Redirect[Page], Page]

  protected case class Rule[Page](
    parse: Path => Option[Parsed[Page]],
    path: Page => Option[Path],
    target: Page => Option[VNode]
  )

  protected class RouterConfig[Page](rules: Seq[Rule[Page]], notFound: Parsed[Page]) {

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


  def onPageChange: (Page => Unit) = _ => ()

  private def parsedToPageWithEffects[S](parsed: Parsed[Page]) : Page  = {
    val page = parsed match {
      case Right(page) =>
        page
      case Left(Redirect(page, true)) =>
        dom.window.history.replaceState("", "", pageToUrl(page).value)
        page
      case Left(Redirect(page, false)) =>
        dom.window.history.pushState("", "", pageToUrl(page).value)
        page
    }
    page
  }

  private def fromEvent(target: dom.EventTarget, event: String): Observable[dom.Event] =
    Observable.create { subscriber =>
      val eventHandler: js.Function1[dom.Event, Unit] = (e: dom.Event) => subscriber.next(e)
      target.addEventListener(event, eventHandler)
      val cancel: Observable.Creator = () => {
        target.removeEventListener(event, eventHandler)
        subscriber.complete()
      }
      cancel
    }

  private val popStateObservable = fromEvent(dom.window, "popstate")
    .startWith(dom.document.createEvent("PopStateEvent"))
    .map { _ => AbsUrl.fromWindow }
    .map(parseUrl)

  val pageChangeSource = popStateObservable
    .merge(
      pageHandler.map(r => Left(r))
    )
    .map(parsedToPageWithEffects)

  private val vnodeSource = pageChangeSource.map { page =>
    onPageChange(page)
    pageToNode(page)
  }

  def baseLayout(vnode: Observable[VNode]): VNode = {
    outwatch.dom.div(outwatch.dom.child <-- vnode)
  }

  def apply(): VNode = baseLayout(vnodeSource)
}
