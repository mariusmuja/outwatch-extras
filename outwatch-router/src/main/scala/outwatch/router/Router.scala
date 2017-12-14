package outwatch.router

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import monix.execution.misc.NonFatal
import org.scalajs.dom
import outwatch.dom.dsl.{styles, tags}
import outwatch.dom.helpers.STRef
import outwatch.dom.{Handler, Observable, Pipe, VNode, WindowEvents, dsl}
import outwatch.extras.>-->

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
  * Created by marius on 26/06/17.
  */

trait Router[Page] {

  sealed trait Action
  case class Push(page: Page) extends Action
  case class Replace(page: Page) extends Action
  case class Force(url: AbsUrl) extends Action

  case class State(page: Option[Page], node: VNode)

  private type Parsed = Either[Action, Page]

  class Config private (rules: Seq[Config.Rule], val notFound: Either[Action, VNode]) {

    @tailrec
    private def findFirst[T, R](list: List[T => Option[R]])(arg: T): Option[R] = {
      list match {
        case Nil => None
        case head :: tail =>
          val res = head(arg)
          if (res.isDefined) res else findFirst(tail)(arg)
      }
    }

    private def parse(path: Path): Option[Parsed] = findFirst(rules.map(_.parse).toList)(path)

    private def path(page: Page): Option[Path] = findFirst(rules.map(_.path).toList)(page)

    private def target(page: Page): Option[VNode] = findFirst(rules.map(_.target).toList)(page)

    def parseUrl(baseUrl: BaseUrl, absUrl: AbsUrl): Option[Parsed] = {
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

  private def invalidConfiguration(page: Page): (Option[Page], VNode) = {
    Some(page) -> tags.div(styles.color.red, s"Invalid configuration, missing rule for ${page.getClass.getName}")
  }

  object Config {

    private[Config] case class Rule(
      parse: Path => Option[Parsed],
      path: Page => Option[Path],
      target: Page => Option[VNode]
    )

    private[Config] case class Builder private(rules: Seq[Rule]) extends PathParser {

      implicit def toVNodeFunc[P](f: => VNode): P => VNode = _ => f

      implicit class route[P <: Page](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {
        def ~>(f: => P => VNode): Rule = Rule(
          p => rf.route.parse(p).map(p => Right(p)),
          p => ct.unapply(p).map(rf.route.pathFor),
          p => ct.unapply(p).map(f)
        )
      }

      implicit class toRedirect[P](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {
        def ~>(f: => Action): Rule = Rule(
          p => rf.route.parse(p).map(p => Left(f)),
          p => ct.unapply(p).map(rf.route.pathFor),
          p => None
        )
      }

      def rules(r: Rule*): Builder = copy(rules = r)

      def notFound(page: Action) = new Config(rules, Left(page))

      def notFound(node: VNode) = new Config(rules, Right(node))
    }

    def apply(builder: Builder => Config): Config =
      builder(Builder(Seq.empty))
  }

  private object RedirectException extends Exception("Redirecting...")

  def create(config: Config, baseUrl: BaseUrl): IO[Action >--> State] = {
    Handler.create[Action]().map { pageHandler =>
      val parsedToPageWithEffects: Parsed => Page = {
        case Right(page) =>
          page
        case Left(Replace(page)) =>
          dom.window.history.replaceState("", "", config.pageToUrl(baseUrl, page).value)
          page
        case Left(Push(page)) =>
          dom.window.history.pushState("", "", config.pageToUrl(baseUrl, page).value)
          page
        case Left(Force(url)) =>
          dom.window.location.href = url.value
          throw RedirectException
      }

      val popStateObservable = dsl.events.window.onPopState
        .startWith(Seq(()))
        .map(_ => config.parseUrl(baseUrl, AbsUrl.fromWindow))

      val pageChanged: Observable[Option[Page]] = Observable.merge(
        popStateObservable,
        pageHandler.map(r => Some(Left(r)))
      )
        .map(_.map(parsedToPageWithEffects))

      def pageToNode(page: Page): Option[(Option[Page], VNode)] = {
        try {
          config.pageToNode(page).map(node => Some(page) -> node)
        } catch {
          case NonFatal(e) =>
            dom.console.error(e.getMessage)
            Some((None, tags.div(styles.color.red, e.getMessage)))
        }
      }

      val source = pageChanged.map { pageOpt =>

        val (page, node): (Option[Page], VNode) = pageOpt.flatMap(pageToNode)
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
      .onErrorFallbackTo(Observable.empty) // in case of a Force redirect

      Pipe(pageHandler, source.share)
    }
  }

  private val ref = STRef.empty[Action >--> State]

  private object NoRouterException extends
    Exception("A router was not created, please use Router.createRef to create the router")

  def createRef(config: Config, baseUrl: BaseUrl): IO[Action >--> State] = {
    create(config, baseUrl).flatMap { router =>
      ref.put(router)
    }
  }

  def get: IO[Action >--> State] = {
    ref.getOrThrow(NoRouterException)
  }
}
