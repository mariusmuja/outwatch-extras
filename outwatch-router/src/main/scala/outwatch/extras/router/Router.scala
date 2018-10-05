package outwatch.extras.router

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import monix.execution.misc.NonFatal
import org.scalajs.dom
import outwatch.dom.dsl.{styles, tags}
import outwatch.dom.helpers.STRef
import outwatch.dom.{Handler, Observable, VNode, dsl}
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
    private def findFirst[T, X](arg: T, rules: List[Config.Rule], map: Config.Rule => T => Option[X]): Option[X] = {
      rules match {
        case Nil => None
        case head :: tail =>
          val res = map(head)(arg)
          if (res.isDefined) res else findFirst(arg, tail, map)
      }
    }


    def pathForPage(page: Page): Option[Path] = findFirst(page, rules.toList, _.path)

    def targetForPage(page: Page): Option[VNode] = findFirst(page, rules.toList, _.target)

    def parsePath(path: Path): Option[Parsed] = findFirst(path, rules.toList, _.parse)

    def parseUrl(baseUrl: BaseUrl, absUrl: AbsUrl): Option[Parsed] = {
      val path = Path(absUrl.value.replaceFirst(baseUrl.value, ""))
      val parsed = parsePath(path)
      parsed
    }

    def urlForPage(baseUrl: BaseUrl, page: Page): AbsUrl = {
      pathForPage(page).getOrElse(Path.root).abs(baseUrl)
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


    trait ActionRule { self: PathParser =>
      implicit def toActionFunc[P](f: => Action): P => Action = _ => f

      implicit class RouteAsAction[P](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {
        private val route = rf.route

        def ~>(f: P => Action): Rule = Rule(
          p => route.parse(p).map(p => Left(f(p))),
          p => ct.unapply(p).map(route.pathFor),
          p => None
        )
      }
    }


    trait VNodeRule extends ActionRule { self: PathParser =>

      implicit def toVNodeFunc[P](f: => VNode): P => VNode = _ => f

      implicit class route[P <: Page](rf: RouteFragment[P])(implicit ct: ClassTag[P]) {

        private val route = rf.route

        def ~>(f: P => VNode): Rule = Rule(
          p => route.parse(p).map(p => Right(p)),
          p => ct.unapply(p).map(route.pathFor),
          p => ct.unapply(p).map(f)
        )
      }
    }

    private[Config] case class Builder private(rules: Seq[Rule]) extends PathParser with VNodeRule {

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
          dom.window.history.replaceState("", "", config.urlForPage(baseUrl, page).value)
          page
        case Left(Push(page)) =>
          dom.window.history.pushState("", "", config.urlForPage(baseUrl, page).value)
          page
        case Left(Force(url)) =>
          dom.window.location.href = url.value
          throw RedirectException
      }

      val popStateObservable: Observable[Option[Parsed]] = dsl.events.window.onPopState
        .startWith(Seq(()))
        .map(_ => config.parseUrl(baseUrl, AbsUrl.fromWindow))

      val pageChanged: Observable[Option[Page]] = Observable.merge(
        popStateObservable,
        pageHandler.map(r => Some(Left(r)))
      )
        .map(_.map(parsedToPageWithEffects))

      def pageToNode(page: Page): Option[(Option[Page], VNode)] = {
        try {
          config.targetForPage(page).map(node => Some(page) -> node)
        } catch {
          case NonFatal(e) =>
            dom.console.error(Option(e.getCause).map(_.getMessage).getOrElse[String](e.getMessage))
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

      pageHandler.transformSource(_ => source.share)
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
