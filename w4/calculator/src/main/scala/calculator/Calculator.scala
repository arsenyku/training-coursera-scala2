package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
  {
    namedExpressions.map({ case (n, e) => n -> Signal( eval(e(), namedExpressions) ) })
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = evalExpr(expr, references, Set())

  def evalExpr(expr: Expr, references: Map[String, Signal[Expr]], names:Set[String]): Double = expr match
  {
    case Literal(v) => v
    case Plus(a,b) => evalExpr(a, references, names) + evalExpr(b, references, names)
    case Minus(a,b) => evalExpr(a, references, names) - evalExpr(b, references, names)
    case Times(a,b) => evalExpr(a, references, names) * evalExpr(b, references, names)
    case Divide(a,b) => evalExpr(a, references, names) / evalExpr(b, references, names)
    case Ref(n) =>
      if (names contains n) Double.NaN
      else evalExpr(getReferenceExpr(n, references), references, names + n)
    case _ => Double.NaN
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }

}
