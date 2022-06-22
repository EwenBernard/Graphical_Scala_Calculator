package ArithmParser

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

abstract class Tree
case class Add(t1: Tree, t2: Tree) extends Tree
case class Sub(t1: Tree, t2: Tree) extends Tree
case class Mul(t1: Tree, t2: Tree) extends Tree
case class Div(t1: Tree, t2: Tree) extends Tree
case class Func(f: String, t: Tree) extends Tree
case class Num(t: Double) extends Tree
case class Variable(t: String) extends Tree


trait ExprParser extends JavaTokenParsers {

  def eval(t: Tree, x: Double): Double = t match {
    case Add(t1, t2) => eval(t1, x) + eval(t2, x)
    case Sub(t1, t2) => eval(t1, x) - eval(t2, x)
    case Mul(t1, t2) => eval(t1, x) * eval(t2, x)
    case Div(t1, t2) => eval(t1, x) / eval(t2, x)
    case Func(f, t) => get_func(f, eval(t, x))
    case Variable(t: String) => x
    case Num(t) => t
  }

  def eval_points(xlim1: Double, xlim2: Double, step: Double, t: Tree): ListBuffer[(Double, Double)] ={
    var points : ListBuffer[(Double, Double)] = ListBuffer()
    for(x <- xlim1 to xlim2 by step){
      val temp = (x,eval(t, x))
      points += temp
    }
    points
  }


  def get_func(f: String, t: Double): Double = f match {
    case "cos" => Math.cos(t)
    case "sin" => Math.sin(t)
    case "tan" => Math.tan(t)
    case "cosh" => Math.cosh(t)
    case "sinh" => Math.sinh(t)
    case "tanh" => Math.tanh(t)
    case "log" => Math.log10(t)
    case "ln" => Math.log(t)
    case "exp" => Math.exp(t)
  }

  lazy val expr: Parser[Tree] = term ~ rep("[+-]".r ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "+" ~ t2) => Add(t1, t2)
      case (t1, "-" ~ t2) => Sub(t1, t2)
    }
  }

  lazy val term = factor ~ rep("[*/]".r ~ factor) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "*" ~ t2) => Mul(t1, t2)
      case (t1, "/" ~ t2) => Div(t1, t2)
    }
  }

  lazy val factor = variable | num | func | "(" ~> expr <~ ")"

  lazy val mathVarRegex : Regex = """[xyz]""".r
  lazy val variable = mathVarRegex ^^ { t => Variable(t)}

  lazy val num = floatingPointNumber ^^ { t => Num(t.toDouble) }

  lazy val funcRegex : Regex = """abs|exp|ln|log|a?(?:sin|cos|tan)h?""".r
  lazy val funcExpr = "(" ~> expr <~ ")"

  lazy val func = funcRegex ~ opt(funcExpr) ^^ {
    case t1 ~ Some(t2) => Func(t1, t2)
  }
}