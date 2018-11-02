package outwatch.extras.router

import monix.reactive.Observable
import outwatch.Pipe
import outwatch.dom.IO
import outwatch.extras.>-->


trait RouterOpsBase {
  type Page

  def config: Router.Config
  def baseUrl: BaseUrl

  object Router extends Router[Page]
}

/**
  * Referential transparent RouterOps
  */
trait RouterOps extends RouterOpsBase {

  lazy val create: IO[Router.Action >--> Router.State] = Router.createRef(config, baseUrl)
  lazy val router: IO[Router.Action >--> Router.State] = Router.get

  lazy val push: IO[Pipe[Page, Router.State]] = router.map(_.mapSink[Page](Router.Push))
  lazy val replace: IO[Pipe[Page, Router.State]] = router.map(_.mapSink[Page](Router.Replace))
  lazy val force: IO[Pipe[AbsUrl, Router.State]] = router.map(_.mapSink[AbsUrl](Router.Force))

  def asEffect[E, A](f: PartialFunction[E, Router.Action]): IO[E >--> A]= router.map { router =>
    router.transformPipe[E, A](_.collect(f))(_ => Observable.empty)
  }
}


/**
  * Non-referential transparent RouterOps
  */
trait RouterOpsUnsafe extends RouterOpsBase {

  lazy val router: Router.Action >--> Router.State = Router.create(config, baseUrl).unsafeRunSync()

  lazy val push: Pipe[Page, Router.State] = router.mapSink[Page](Router.Push)
  lazy val replace: Pipe[Page, Router.State] = router.mapSink[Page](Router.Replace)
  lazy val force: Pipe[AbsUrl, Router.State] = router.mapSink[AbsUrl](Router.Force)

  def asEffect[E, A](f: PartialFunction[E, Router.Action]): E >--> A =
    router.transformPipe[E, A](_.collect(f))(_ => Observable.empty)
}