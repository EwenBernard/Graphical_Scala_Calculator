package Arithm

object Utils extends ExprParser {
  def showExpr(e: Tree): String = e match {
    case Num(value) => value.toString
    case Variable(x) => x
    case Add(left,right) => "(" + showExpr(left) + "+" + showExpr(right) + ")"
    case Sub(left, right) => "(" + showExpr(left) + "-" + showExpr(right) + ")"
    case Mul(left,right) => "(" + showExpr(left) + "*" + showExpr(right) + ")"
    case Div(left, right) => "(" + showExpr(left) + "/" + showExpr(right) + ")"
    case Func(f, args) => f + "(" + showExpr(args) + ")"
    case _ => ""
  }
}
