package outwatch.extras.router

import java.util.UUID
import java.util.regex.{Matcher, Pattern}

import scala.language.{higherKinds, implicitConversions}
import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.{Reverse, Tupler}


trait PathParser {

  private val regexEscape1 = """([-()\[\]{}+?*.$\^|,:#<!\\])""".r
  private val regexEscape2 = """\x08""".r

  /**
    * Pattern.quote doesn't work in Scala.JS.
    *
    * http://stackoverflow.com/questions/2593637/how-to-escape-regular-expression-in-javascript
    */
  private def regexEscape(s: String): String = {
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
      type Out
      val ga: Out => A
      val gb: Out => B
      val gc: (A, B) => Out

      def apply(fa: RouteFragment[A], fb: RouteFragment[B]): RouteFragment[Out] =
        new RouteFragment(
          fa.regex + fb.regex,
          fa.matchGroups + fb.matchGroups,
          g => for {a <- fa.parse(g); b <- fb.parse(i => g(i + fa.matchGroups))} yield gc(a, b),
          c => fa.build(ga(c)) + fb.build(gb(c))
        )
    }

    object Composition {

      type Aux[A, B, O] = Composition[A, B] {type Out = O}

      implicit def appendItem[L <: HList, H]: Composition.Aux[L, H, H :: L] =
        Composition[L, H, H :: L](_.tail, _.head, (l, h) => h :: l)

      implicit def appendHNil[A]: Aux[A, HNil, A] = Composition[A, HNil, A](identity, _ => HNil, (a, _) => a)

      def apply[A, B, O](a: O => A, b: O => B, c: (A, B) => O): Aux[A, B, O] = new Composition[A, B] {
        type Out = O
        val ga = a
        val gb = b
        val gc = c
      }
    }

    def literal(s: String): RouteFragment[HNil] = new RouteFragment(regexEscape(s), 0, _ => Option(HNil), _ => s)

    val / : RouteFragment[HNil] = literal("/")
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
    final def xmap[B](b: A => B)(a: B => A): R[B] = pmap(a => Some(b(a)))(a)

    final def filter(f: A => Boolean): R[A] = parseThen(_ filter f)

    final def mapParsed[B <: A](f: A => B): R[B] = xmap(f)(x => x)

    final def mapInput[B >: A](f: B => A): R[B] = xmap[B](x => x)(f)

    final def const[B](b: B)(implicit ev: HNil =:= A): R[B] = xmap(_ => b)(_ => HNil)
  }

  implicit class RouteConversion[A <: HList, R <: HList](route: RouteFragment[A])(
    implicit
    reverse: Reverse.Aux[A, R],
    reverse2: Reverse.Aux[R, A]
  ) {
    def mapTo[B](implicit gen: Generic.Aux[B, R]): RouteFragment[B] =
      route.xmap(a => gen.from(reverse(a)))(b => reverse2(gen.to(b)))

    def tupled[T](implicit tupler: Tupler.Aux[R, T], gen: Generic.Aux[T, R]): RouteFragment[T] =
      route.xmap(a => tupler(reverse(a)))(b => reverse2(gen.to(b)))

    @deprecated("Use .mapTo[T] instead", "0.2.3")
    def caseClass[B](implicit genB: Generic.Aux[B, R]): RouteFragment[B] = mapTo[B]
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

    override def toString = s"RouteFragment($regex)"

    def ~[B](next: RouteFragment[B])(implicit c: Composition[A, B]): RouteFragment[c.Out] =
      c(this, next)

    def /[B](next: RouteFragment[B])(implicit c: Composition[A, B]): RouteFragment[c.Out] =
      c(this ~ RouteFragment./, next)

    override def parseThen(f: Option[A] => Option[A]): RouteFragment[A] =
      new RouteFragment(regex, matchGroups, f compose parse, build)

    override def pmap[B](b: A => Option[B])(a: B => A): RouteFragment[B] =
      new RouteFragment(regex, matchGroups, parse(_) flatMap b, build compose a)

    def option: RouteFragment[Option[A]] = new RouteFragment[Option[A]](
      s"($regex)?",
      matchGroups + 1,
      g => Some(if (g(0) eq null) None else parse(i => g(i + 1))), _.fold("")(build)
    )

    final def route: Route[A] = {
      val p = Pattern.compile("^" + regex + "$")
      // https://github.com/scala-js/scala-js/issues/1727
      // val g = p.matcher("").groupCount
      // if (g != matchGroups)
      //   sys.error(s"Error in regex: /${p.pattern}/. Expected $matchGroups match groups but detected $g.")
      new Route(p, m => parse(i => m.group(i + 1)), a => Path(build(a)))
    }
  }


  implicit class RouteFragmentOption[A](private val r: RouteFragment[Option[A]])  {

    /**
      * Specify a default value when parsing.
      *
      * Eg. If the path is like "/file[.format]" and the default is JSON, "/file" will be read as "/file.json", but
      * when generating a path with JSON this will generate "/file.json" instead of "/file".
      */
    def parseDefault(default: => A): RouteFragment[A] = r.xmap(_ getOrElse default)(Some(_))

    /**
      * Specify a default value.
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
  final class Route[A](
    pattern: Pattern,
    parseFn: Matcher => Option[A],
    buildFn: A => Path
  ) extends RouteCommon[Route, A] {
    override def toString =
      s"Route($pattern)"

    override def parseThen(f: Option[A] => Option[A]): Route[A] =
      new Route(pattern, f compose parseFn, buildFn)

    override def pmap[B](b: A => Option[B])(a: B => A): Route[B] =
      new Route(pattern, parseFn(_) flatMap b, buildFn compose a)

    def parse(path: Path): Option[A] = {
      val m = pattern.matcher(path.value)
      if (m.matches) parseFn(m) else None
    }

    def pathFor(a: A): Path = buildFn(a)
  }


  private val intRegex = "(-?\\d+)"
  private val doubleRegex = "(-?\\d+\\.?\\d*)"
  private val uuidRegex = "([A-Fa-f0-9]{8}(?:-[A-Fa-f0-9]{4}){3}-[A-Fa-f0-9]{12})"

  def root: Path = Path.root
  val int = new RouteFragment[Int](intRegex, 1, g => Some(g(0).toInt), _.toString)
  val long = new RouteFragment[Long](intRegex, 1, g => Some(g(0).toLong), _.toString)
  val double = new RouteFragment[Double](doubleRegex, 1, g => Some(g(0).toDouble), _.toString)
  val uuid = new RouteFragment[UUID](uuidRegex, 1, g => Some(UUID.fromString(g(0))), _.toString)

  private def stringRouteFragment(regex: String): RouteFragment[String] =
    new RouteFragment(regex, 1, g => Some(g(0)), identity)

  /**
    * Matches a string.
    *
    * Best to use a whitelist of characters, eg. "[a-zA-Z0-9]+".
    * Do not capture groups; use "[a-z]+" instead of "([a-z]+)".
    * If you need to group, use non-capturing groups like "(?:bye|hello)" instead of "(bye|hello)".
    */
  def string(regex: String): RouteFragment[String] =
    stringRouteFragment("(" + regex + ")")

  /** Captures the (non-empty) remaining portion of the URL path. */
  def remainingPath: RouteFragment[String] =
    stringRouteFragment("(.+)$")

  /** Captures the (potentially-empty) remaining portion of the URL path. */
  def remainingPathOrBlank: RouteFragment[String] =
    stringRouteFragment("(.*)$")


  implicit def routeFragmentFromString(l: String): RouteFragment[HNil] = RouteFragment.literal(l)
  implicit def routeFragmentFromPath(p: Path): RouteFragment[HNil] = RouteFragment.literal(p.value)

}
