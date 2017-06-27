package outwatch.extras.router

import org.scalajs.dom
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

  protected trait Page
  object Page {
    case class Replace(page: Page) extends Page
    implicit class toReplace(p: Page) {
      def withReplace = Replace(p)
    }
  }

  type PageSink = Sink[Page]

  protected val routerActions = Handlers.createHandler[Page]()


  protected case class Redirect[P](page: P)

  protected type Parsed[Page] = Either[Redirect[Page], Page]

  protected case class Rule[Page](
    parse: Path => Option[Parsed[Page]],
    path: Page => Option[Path],
    target: Page => Option[VNode]
  )

  protected class RouterConfig[Page](rules: Seq[Rule[Page]], val notFound: Parsed[Page]) {

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

    def target(page: Page): Option[VNode] = findFirst(rules.map(_.target).toList)(page)
  }

  protected object RouterConfig {

    case class RouterConfigBuilder[Page](rules: Seq[Rule[Page]]) extends PathParser {

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



  protected val config : RouterConfig[Page]

  val baseUrl : BaseUrl

  private def parseUrl(absUrl: AbsUrl): Parsed[Page] = {
    val path = Path(absUrl.value.replaceFirst(baseUrl.value, ""))
    val parsed = config.parse(path).getOrElse(
      config.notFound
    )
    parsed
  }

  private def pageToUrl(page: Page): AbsUrl = {
    val path = config.path(page).getOrElse(Path.root)
    path.abs(baseUrl)
  }

  private def pageToNode(page: Page) : VNode = {
    config.target(page).getOrElse {
      outwatch.dom.div(s"Page not found, check the routes config")
    }
  }

  private def parsedToNodeWithEffects[S](parsed: Parsed[Page]) : VNode = {
    parsed match {
      case Right(page) =>
        pageToNode(page)
      case Left(Redirect(Page.Replace(page))) =>
        dom.window.history.replaceState("", "", pageToUrl(page).value)
        pageToNode(page)
      case Left(Redirect(page)) =>
        dom.window.history.pushState("", "", pageToUrl(page).value)
        pageToNode(page)
    }
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


  private val nodes = popStateObservable
    .merge(
      routerActions.map(p => Left(Redirect(p)))
    )
    .map(parsedToNodeWithEffects)

  def baseLayout(node: Observable[VNode]): VNode = {
    outwatch.dom.div(outwatch.dom.child <-- node)
  }

  def apply(): VNode = baseLayout(nodes)
}
