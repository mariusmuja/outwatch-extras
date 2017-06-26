package outwatch.extras.router

import java.util.UUID
import java.util.regex.{Matcher, Pattern}

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}



/**
  * Created by marius on 13/06/17.
  */
trait PathParser {

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
          c => fa.build(ga(c)) + fb.build(gb(c))
        )
    }

    trait Composition_PriLowest {
      implicit def T2[A, B] = Composition[A, B, (A, B)](_._1, _._2, (_, _))
    }

    trait Composition_PriLow extends Composition_PriLowest {
      implicit def T8[A, B, C, D, E, F, G, H] = Composition[(A, B, C, D, E, F, G), H, (A, B, C, D, E, F, G, H)](
        r => (r._1, r._2, r._3, r._4, r._5, r._6, r._7), _._8, (l, r) => (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r)
      )

      implicit def T7[A, B, C, D, E, F, G] = Composition[(A, B, C, D, E, F), G, (A, B, C, D, E, F, G)](
        r => (r._1, r._2, r._3, r._4, r._5, r._6), _._7, (l, r) => (l._1, l._2, l._3, l._4, l._5, l._6, r)
      )

      implicit def T6[A, B, C, D, E, F] = Composition[(A, B, C, D, E), F, (A, B, C, D, E, F)](
        r => (r._1, r._2, r._3, r._4, r._5), _._6, (l, r) => (l._1, l._2, l._3, l._4, l._5, r)
      )

      implicit def T5[A, B, C, D, E] = Composition[(A, B, C, D), E, (A, B, C, D, E)](
        r => (r._1, r._2, r._3, r._4), _._5, (l, r) => (l._1, l._2, l._3, l._4, r)
      )

      implicit def T4[A, B, C, D] = Composition[(A, B, C), D, (A, B, C, D)](
        r => (r._1, r._2, r._3), _._4, (l, r) => (l._1, l._2, l._3, r)
      )

      implicit def T3[A, B, C] = Composition[(A, B), C, (A, B, C)](
        r => (r._1, r._2), _._3, (l, r) => (l._1, l._2, r)
      )
    }

    trait Composition_PriMed extends Composition_PriLow {
      implicit def unitToOther[A] = Composition[Unit, A, A](_ => (), identity, (_, a) => a)

      implicit def otherToUnit[A] = Composition[A, Unit, A](identity, _ => (), (a, _) => a)
    }

    object Composition extends Composition_PriMed {
      implicit def unitToUnit = Composition[Unit, Unit, Unit](_ => (), _ => (), (_, _) => ())

      def apply[A, B, O](a: O => A, b: O => B, c: (A, B) => O) =
        new Composition[A, B] {
          override type C = O
          val ga = a
          val gb = b
          val gc = c
        }
    }

    def literal(s: String): RouteFragment[Unit] =
      new RouteFragment(regexEscape(s), 0, _ => Option(()), _ => s)

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
      s"RouteFragment($regex)"

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
    def caseClass[B]: RouteFragment[B] = macro RouterMacros.quietCaseClass[RouteFragment, B]

    /**
      * Same as [[caseClass]] except the code generated by the macro is printed to stdout.
      */
    def caseClassDebug[B]: RouteFragment[B] = macro RouterMacros.debugCaseClass[RouteFragment, B]

    def option: RouteFragment[Option[A]] = new RouteFragment[Option[A]](
      s"($regex)?", matchGroups + 1,
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

  class RouteFragmentOption[A](private val r: RouteFragment[Option[A]])  {

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
    def caseClass[B]: Route[B] = macro RouterMacros.quietCaseClass[Route, B]

    /**
      * Same as [[caseClass]] except the code generated by the macro is printed to stdout.
      */
    def caseClassDebug[B]: Route[B] = macro RouterMacros.debugCaseClass[Route, B]

    def parse(path: Path): Option[A] = {
      val m = pattern.matcher(path.value)
      if (m.matches) parseFn(m) else None
    }

    def pathFor(a: A): Path = buildFn(a)
  }


  private def uuidRegex = "([A-Fa-f0-9]{8}(?:-[A-Fa-f0-9]{4}){3}-[A-Fa-f0-9]{12})"

  def root: Path = Path.root
  val int = new RouteFragment[Int]("(-?\\d+)", 1, g => Some(g(0).toInt), _.toString)
  val long = new RouteFragment[Long]("(-?\\d+)", 1, g => Some(g(0).toLong), _.toString)
  val uuid = new RouteFragment[UUID](uuidRegex, 1, g => Some(UUID fromString g(0)), _.toString)

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



  implicit def convertRouteFragmentOption[A](r: RouteFragment[Option[A]]): RouteFragmentOption[A] = new RouteFragmentOption(r)

  implicit def routeFragmentFromString(l: String): RouteFragment[Unit] = RouteFragment.literal(l)
  implicit def routeFragmentFromPath(p: Path): RouteFragment[Unit] = RouteFragment.literal(p.value)

}
