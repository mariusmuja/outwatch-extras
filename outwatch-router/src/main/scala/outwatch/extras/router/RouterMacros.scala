package outwatch.extras.router

import scala.language.higherKinds
import scala.reflect.macros.blackbox.Context

class RouterMacros(val c: Context) extends MacroUtils {
  import c.universe._

  def quietCaseClass[R[_], T: c.WeakTypeTag]: c.Expr[R[T]] = caseClass[R, T](debug = false)
  def debugCaseClass[R[_], T: c.WeakTypeTag]: c.Expr[R[T]] = caseClass[R, T](debug = true)

  private def caseClass[R[_], T: c.WeakTypeTag](debug: Boolean): c.Expr[R[T]] = {


    val T       = caseClassType[T]
    val params  = primaryConstructorParams(T)
    val applyFn = tcApplyFn(T)

    def xmap  = replaceMacroCallWith("xmap")
    def const = replaceMacroCallWith("const")

    val impl =
      params match {
        case Nil =>
          q"$const[$T]($applyFn())"

        case param :: Nil =>
          val (n, t) = nameAndType(T, param)
          q"$xmap[$T]($applyFn)(_.$n)"

        case _ =>
          var fromTuple = Vector.empty[Tree]
          var toTuple   = Vector.empty[Tree]
          var index     = 0
          for (p <- params) {
            index += 1
            val (n, t) = nameAndType(T, p)
            val tn = TermName("_" + index)
            fromTuple :+= q"t.$tn"
            toTuple   :+= q"c.$n"
          }
          q"$xmap[$T](t => $applyFn(..$fromTuple))(c => (..$toTuple))"
      }

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[R[T]](impl)
  }
}
