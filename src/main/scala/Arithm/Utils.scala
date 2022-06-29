package Arithm

object Utils extends ExprParser {
  def showExpr(e: Tree): String = e match {
    case Num(value) => value.toString
    case Variable(x) => x
    case Mul(Num(t1), Variable(t2)) => t1.toString + "*" + t2
    case Add(left, right) if left == Num(0) && right != Num(0) => "(" + showExpr(right) + ")"
    case Add(left, right) if right == Num(0) && left != Num(0) => "(" + showExpr(left) + ")"
    case Add(left,right) if  right != Num(0) && left != Num(0) => "(" + showExpr(left) + "+" + showExpr(right) + ")"
    case Sub(left, right) if left == Num(0) => "-" + showExpr(right)
    case Sub(left, right) if left != Num(0) => "(" + showExpr(left) + "-" + showExpr(right) + ")"
    case Mul(left,right) => "(" + showExpr(left) + "*" + showExpr(right) + ")"
    case Div(left, right) => "(" + showExpr(left) + "/" + showExpr(right) + ")"
    case Func(f, args) => f + "(" + showExpr(args) + ")"
    case _ => ""
  }

  def trimExpr(s: String): String = {
    if(s.charAt(0) == "("){
      print(s.charAt(0))
      s.substring( 1, s.length() - 1 )
    }
    else{
      s
    }

  }

}
