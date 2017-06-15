package outwatch.extras

import java.util.UUID
import java.util.regex.{Matcher, Pattern}

import org.scalajs.dom

import scala.annotation.elidable



abstract class PathLike[Self <: PathLike[Self]] { this: Self =>

  @elidable(elidable.ASSERTION)
  def assertWarn(test: => Boolean, msg: => String): Unit =
    if (!test)
      dom.console.warn(msg)

  protected def make(s: String): Self
  protected def str(s: Self): String

  final def map(f: String => String): Self = make(f(str(this)))
  final def +(p: String): Self = map(_ + p)
  final def +(p: Self): Self = this + str(p)
  final def /(p: String): Self = endWith_/ + p
  final def /(p: Self): Self = this / str(p)
  final def endWith_/ : Self = map(_.replaceFirst("/*$", "/"))
  final def rtrim_/ : Self = map(_.replaceFirst("/+$", ""))
  final def isEmpty: Boolean = str(this).isEmpty
  final def nonEmpty: Boolean = str(this).nonEmpty
}

/**
  * The prefix of all routes on a page.
  *
  * The router expects this to be a full URL.
  * Examples: `BaseUrl("http://www.blah.com/hello")`,  `BaseUrl.fromWindowOrigin / "hello"`.
  */
final case class BaseUrl(value: String) extends PathLike[BaseUrl] {
  assertWarn(value contains "://",
    s"$this doesn't seem to be a valid URL. It's missing '://'. Consider using BaseUrl.fromWindowOrigin.")

  override protected def make(s: String) = BaseUrl(s)
  override protected def str(s: BaseUrl) = s.value

  def apply(p: Path): AbsUrl = AbsUrl(value + p.value)
  def abs: AbsUrl = AbsUrl(value)
}

object BaseUrl {
  def fromWindowOrigin: BaseUrl = {
    val l = dom.window.location
    var url = l.protocol + "//" + l.hostname
    if (!l.port.matches("^(?:80)?$"))
      url += ":" + l.port
    BaseUrl(url)
  }

  def fromWindowOrigin_/ : BaseUrl =
    fromWindowOrigin.endWith_/

  def fromWindowUrl(f: String => String): BaseUrl =
    BaseUrl(f(dom.window.location.href))

  def until(stopAt: String): BaseUrl =
    fromWindowUrl { u =>
      val i = u indexOf stopAt
      if (i < 0) u else u.take(i)
    }

  def until_# : BaseUrl =
    until("#")
}

/**
  * The portion of the URL after the [[BaseUrl]].
  */
final case class Path(value: String) extends PathLike[Path] {
  override protected def make(s: String) = Path(s)
  override protected def str(s: Path) = s.value

  def abs(implicit base: BaseUrl): AbsUrl = base apply this

  /**
    * Attempts to remove an exact prefix and return a non-empty suffix.
    */
  def removePrefix(prefix: String): Option[Path] = {
    val l = prefix.length
    if (value.length > l && value.startsWith(prefix))
      Some(Path(value substring l))
    else
      None
  }
}

object Path {
  def root = Path("")
}

final case class AbsUrl(value: String) extends PathLike[AbsUrl] {
  assertWarn(value contains "://",
    s"$this doesn't seem to be a valid URL. It's missing '://'. Consider using AbsUrl.fromWindow.")
  override protected def make(s: String) = AbsUrl(s)
  override protected def str(s: AbsUrl) = s.value
}

object AbsUrl {
  def fromWindow = AbsUrl(dom.window.location.href)
}



/**
  * Created by marius on 13/06/17.
  */
trait PathParser {

  private[this] val identityFnInstance: Any => Any =
    a => a

  def identityFn[A]: A => A =
    identityFnInstance.asInstanceOf[A => A]


  private val regexEscape1 = """([-()\[\]{}+?*.$\^|,:#<!\\])""".r
  private val regexEscape2 = """\x08""".r

  /**
    * Pattern.quote doesn't work in Scala.JS.
    *
    * http://stackoverflow.com/questions/2593637/how-to-escape-regular-expression-in-javascript
    */
  def regexEscape(s: String): String = {
    var r = s
    r = regexEscape1.replaceAllIn(r, """\\$1""")
    r = regexEscape2.replaceAllIn(r, """\\x08""")
    r
  }

  /**
    * Route builder. Allows you to specify routes like `"user" / int / "display"`.
    * Once complete, [[RouteFragment]] will become a [[Route]].
    */
  object RouteFragment {

    trait Composition[A, B] {
      type C
      val ga: C => A
      val gb: C => B
      val gc: (A, B) => C

      def apply(fa: RouteFragment[A], fb: RouteFragment[B]): RouteFragment[C] =
        new RouteFragment(
          fa.regex + fb.regex,
          fa.matchGroups + fb.matchGroups,
          g => for {a <- fa.parse(g); b <- fb.parse(i => g(i + fa.matchGroups))} yield gc(a, b),
          c => fa.build(ga(c)) + fb.build(gb(c)))
    }

    trait Composition_PriLowest {
      implicit def ***[A, B] = Composition[A, B, (A, B)](_._1, _._2, (_, _))
    }

    trait Composition_PriLow extends Composition_PriLowest {
      implicit def T8[A, B, C, D, E, F, G, H] = Composition[(A, B, C, D, E, F, G), H, (A, B, C, D, E, F, G, H)](r => (r._1, r._2, r._3, r._4, r._5, r._6, r._7),
        _._8,
        (l, r) => (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r))

      implicit def T7[A, B, C, D, E, F, G] = Composition[(A, B, C, D, E, F), G, (A, B, C, D, E, F, G)](r => (r._1, r._2, r._3, r._4, r._5, r._6),
        _._7,
        (l, r) => (l._1, l._2, l._3, l._4, l._5, l._6, r))

      implicit def T6[A, B, C, D, E, F] = Composition[(A, B, C, D, E), F, (A, B, C, D, E, F)](r => (r._1, r._2, r._3, r._4, r._5),
        _._6,
        (l, r) => (l._1, l._2, l._3, l._4, l._5, r))

      implicit def T5[A, B, C, D, E] = Composition[(A, B, C, D), E, (A, B, C, D, E)](r => (r._1, r._2, r._3, r._4),
        _._5,
        (l, r) => (l._1, l._2, l._3, l._4, r))

      implicit def T4[A, B, C, D] = Composition[(A, B, C), D, (A, B, C, D)](r => (r._1, r._2, r._3),
        _._4,
        (l, r) => (l._1, l._2, l._3, r))

      implicit def T3[A, B, C] = Composition[(A, B), C, (A, B, C)](r => (r._1, r._2), _._3, (l, r) => (l._1, l._2, r))
    }

    trait Composition_PriMed extends Composition_PriLow {
      implicit def _toA[A] = Composition[Unit, A, A](_ => (), identityFn, (_, a) => a)

      implicit def Ato_[A] = Composition[A, Unit, A](identityFn, _ => (), (a, _) => a)
    }

    object Composition extends Composition_PriMed {
      implicit def _to_ = Composition[Unit, Unit, Unit](_ => (), _ => (), (_, _) => ())

      type Aux[A, B, O] = Composition[A, B] {type C = O}

      def apply[A, B, O](a: O => A, b: O => B, c: (A, B) => O): Aux[A, B, O] =
        new Composition[A, B] {
          override type C = O
          val ga = a
          val gb = b
          val gc = c
        }
    }

    private val someUnit = Some(())

    def literal(s: String): RouteFragment[Unit] =
      new RouteFragment(regexEscape(s), 0, _ => someUnit, _ => s)

    val / = literal("/")
  }

  abstract class RouteCommon[R[X] <: RouteCommon[R, X], A] {

    def parseThen(f: Option[A] => Option[A]): R[A]

    /**
      * Prism map.
      *
      * Some values of `A` can be turned into a `B`s, some fail (in which case the route is considered non-matching).
      *
      * All `B`s can be turned back into `A`s.
      */
    def pmap[B](b: A => Option[B])(a: B => A): R[B]

    /**
      * Exponential map.
      *
      * Any `A` can be turned into a `B` and vice versa.
      */
    final def xmap[B](b: A => B)(a: B => A): R[B] =
      pmap(a => Some(b(a)))(a)

    final def filter(f: A => Boolean): R[A] =
      parseThen(_ filter f)

    final def mapParsed[B <: A](f: A => B): R[B] =
      xmap(f)(x => x)

    final def mapInput[B >: A](f: B => A): R[B] =
      xmap[B](x => x)(f)

    final def const[B](b: B)(implicit ev: A =:= Unit, ev2: Unit =:= A): R[B] =
      xmap(_ => b)(_ => ())
  }

  /**
    * A fragment of a route. Can be composed with other fragments.
    *
    * @param matchGroups The number of matches that `regex` will capture.
    */
  class RouteFragment[A](val regex: String,
    val matchGroups: Int,
    val parse: (Int => String) => Option[A],
    val build: A => String
  ) extends RouteCommon[RouteFragment, A] {

    import RouteFragment.Composition

    override def toString =
      s"RouteB($regex)"

    def ~[B](next: RouteFragment[B])(implicit c: Composition[A, B]): RouteFragment[c.C] =
      c(this, next)

    def /[B](next: RouteFragment[B])(implicit c: Composition[A, B]): RouteFragment[c.C] =
      this ~ RouteFragment./ ~ next

    override def parseThen(f: Option[A] => Option[A]): RouteFragment[A] =
      new RouteFragment(regex, matchGroups, f compose parse, build)

    override def pmap[B](b: A => Option[B])(a: B => A): RouteFragment[B] =
      new RouteFragment(regex, matchGroups, parse(_) flatMap b, build compose a)

    /**
      * Maps the captures values of the route to a case class.
      */
    //    def caseClass[B]: RouteB[B] =
    //    macro RouterMacros.quietCaseClassB[B]

    /**
      * Same as [[caseClass]] except the code generated by the macro is printed to stdout.
      */
    //    def caseClassDebug[B]: RouteB[B] =
    //    macro RouterMacros.debugCaseClassB[B]

    def option: RouteFragment[Option[A]] =
    new RouteFragment[Option[A]](s"($regex)?", matchGroups + 1,
      g => Some(if (g(0) eq null) None else parse(i => g(i + 1))),
      _.fold("")(build))

    final def route: Route[A] = {
      val p = Pattern.compile("^" + regex + "$")
      // https://github.com/scala-js/scala-js/issues/1727
      // val g = p.matcher("").groupCount
      // if (g != matchGroups)
      //   sys.error(s"Error in regex: /${p.pattern}/. Expected $matchGroups match groups but detected $g.")
      new Route(p, m => parse(i => m.group(i + 1)), a => Path(build(a)))
    }
  }

  class RouteBO[A](private val r: RouteFragment[Option[A]])  {

    /**
      * Specify a default value when parsing.
      *
      * Note: Unlike [[withDefault()]] path generation will still explicitly include the default value.
      *
      * Eg. If the path is like "/file[.format]" and the default is JSON, "/file" will be read as "/file.json", but
      * when generating a path with JSON this will generate "/file.json" instead of "/file".
      */
    def parseDefault(default: => A): RouteFragment[A] =
      r.xmap(_ getOrElse default)(Some(_))

    /**
      * Specify a default value.
      *
      * Note: Unlike [[parseDefault()]] this will affect path generation too.
      *
      * Eg. If the path is like "/file[.format]" and the default is JSON, "/file" will be read as "/file.json", and
      * when generating a path with JSON this will generate "/file" instead of "/file.json".
      *
      * Make sure the type has a useful `.equals()` implementation.
      * Example: `default == default` should be `true`.
      */
    def withDefault(default: => A): RouteFragment[A] =
      r.xmap(_ getOrElse default)(a => if (default == a) None else Some(a))
  }

  /**
    * A complete route.
    */
  final class Route[A](pattern: Pattern,
    parseFn: Matcher => Option[A],
    buildFn: A => Path
  ) extends RouteCommon[Route, A] {
    override def toString =
      s"Route($pattern)"

    override def parseThen(f: Option[A] => Option[A]): Route[A] =
      new Route(pattern, f compose parseFn, buildFn)

    override def pmap[B](b: A => Option[B])(a: B => A): Route[B] =
      new Route(pattern, parseFn(_) flatMap b, buildFn compose a)

    /**
      * Maps the captures values of the route to a case class.
      */
    //    def caseClass[B]: Route[B] =
    //    macro RouterMacros.quietCaseClass[B]

    /**
      * Same as [[caseClass]] except the code generated by the macro is printed to stdout.
      */
    //    def caseClassDebug[B]: Route[B] =
    //    macro RouterMacros.debugCaseClass[B]

    def parse(path: Path): Option[A] = {
      val m = pattern.matcher(path.value)
      if (m.matches)
        parseFn(m)
      else
        None
    }

    def pathFor(a: A): Path =
      buildFn(a)
  }


  private def uuidRegex = "([A-Fa-f0-9]{8}(?:-[A-Fa-f0-9]{4}){3}-[A-Fa-f0-9]{12})"

  def root = Path.root
  val int  = new RouteFragment[Int] ("(-?\\d+)", 1, g => Some(g(0).toInt),           _.toString)
  val long = new RouteFragment[Long]("(-?\\d+)", 1, g => Some(g(0).toLong),          _.toString)
  val uuid = new RouteFragment[UUID](uuidRegex,  1, g => Some(UUID fromString g(0)), _.toString)

  private def __string1(regex: String): RouteFragment[String] =
    new RouteFragment(regex, 1, g => Some(g(0)), identityFn)

  /**
    * Matches a string.
    *
    * Best to use a whitelist of characters, eg. "[a-zA-Z0-9]+".
    * Do not capture groups; use "[a-z]+" instead of "([a-z]+)".
    * If you need to group, use non-capturing groups like "(?:bye|hello)" instead of "(bye|hello)".
    */
  def string(regex: String): RouteFragment[String] =
    __string1("(" + regex + ")")

  /** Captures the (non-empty) remaining portion of the URL path. */
  def remainingPath: RouteFragment[String] =
    __string1("(.+)$")

  /** Captures the (potentially-empty) remaining portion of the URL path. */
  def remainingPathOrBlank: RouteFragment[String] =
    __string1("(.*)$")

  implicit def _ops_for_routeb_option[A](r: RouteFragment[Option[A]]) = new RouteBO(r)

  implicit def _auto_routeB_from_str(l: String) = RouteFragment.literal(l)
  implicit def _auto_routeB_from_path(p: Path) = RouteFragment.literal(p.value)
  implicit def _auto_route_from_routeB[A, R <% RouteFragment[A]](r: R) = r.route

}
