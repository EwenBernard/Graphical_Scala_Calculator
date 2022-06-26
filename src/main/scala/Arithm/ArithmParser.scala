package Arithm

import javax.swing.{JFrame, JOptionPane}
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

  def parseInput(input: String) : Tree =
    parseAll(expr, input) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>{
        val message = "EXPRESSION ERROR: Invalid Expression"
        JOptionPane.showMessageDialog(new JFrame(), message, "ERROR",
          JOptionPane.ERROR_MESSAGE)
        throw ExpressionException(message)
      }
    }

  def eval(t: Tree, x: Double): Double = t match {
    case Add(t1, t2) => eval(t1, x) + eval(t2, x)
    case Sub(t1, t2) => eval(t1, x) - eval(t2, x)
    case Div(t1, t2) => {
      if (t1 == Num(0) || t2 == Num(0)){
        val message = "MATHS ERROR: Can't divide by 0"
        JOptionPane.showMessageDialog(new JFrame(), message, "ERROR",
          JOptionPane.ERROR_MESSAGE)
        throw MathException(message)
      }
      else{
        eval(t1, x) / eval(t2, x)
      }
    }
    case Mul(t1, t2) => eval(t1, x) * eval(t2, x)
    case Func(f, t) => get_func(f, eval(t, x))
    case Variable(t: String) => x
    case Num(t) => t
  }

  def eval_points(xlim1: Double, xlim2: Double, step: Double, t: Tree): (Seq[Double], Seq[Double]) ={
    var pointsX : ListBuffer[Double] = ListBuffer()
    var pointsY : ListBuffer[Double] = ListBuffer()
    for(x <- xlim1 to xlim2 by step){
      pointsX += x
      pointsY += eval(t, x)
    }
    (pointsX.toSeq, pointsY.toSeq)
  }

  def get_func(f: String, t: Double): Double = f match {
    case "cos" => Math.cos(t)
    case "sin" => Math.sin(t)
    case "tan" => Math.tan(t)
    case "cosh" => Math.cosh(t)
    case "sinh" => Math.sinh(t)
    case "tanh" => Math.tanh(t)
    case "exp" => Math.exp(t)
    case "pow2" => Math.pow(t, 2)
    case "pow3" => Math.pow(t, 3)
    case "sqrt"=> {
      if (t < 0) {
        val message = "MATHS ERROR: Negative sqrt"
        JOptionPane.showMessageDialog(new JFrame(), message, "ERROR",
          JOptionPane.ERROR_MESSAGE)
        throw MathException(message)
      }
      else{
        Math.sqrt(t)
      }
    }
    case "log" => {
      if (t < 0) {
        val message = "MATHS ERROR: Negative log10"
        JOptionPane.showMessageDialog(new JFrame(), message, "ERROR",
          JOptionPane.ERROR_MESSAGE)
        throw MathException(message)
      }
      else{
        Math.log10(t)
      }
    }
    case "ln" => {
      if (t < 0) {
        val message = "MATHS ERROR: Negative ln"
        JOptionPane.showMessageDialog(new JFrame(), message, "ERROR",
          JOptionPane.ERROR_MESSAGE)
        throw MathException(message)
      }
      else{
        Math.log(t)
      }
    }
    case _ => {
      val message = "EXPRESSION ERROR: Invalid Function"
      JOptionPane.showMessageDialog(new JFrame(), message, "ERROR",
        JOptionPane.ERROR_MESSAGE)
      throw ExpressionException(message)
    }
  }

  def derivate(t: Tree): Tree = t match{
    //Function Derivation
    case Func("pow2", t1) => Mul(Num(2),Mul(t1, derivate(t1)))
    case Func("pow3", t1) => Mul(Num(3), Mul(derivate(t1), Func("pow2", t1)))
    case Func("ln", t1) => Div(derivate(t1), t1)
    case Func("log", t1) => Div(derivate(t1), Mul(t1, Func("ln", Num(10))))
    case Func("exp", t1) => Mul(derivate(t1), Func("exp", t1))
    case Func("sqrt", t1) => Div(derivate(t1), Mul(Num(2), Func("sqrt", t1)))
    case Func("sin", t1) => Mul(derivate(t1), Func("cos", t1))
    case Func("cos", t1) => Sub(Num(0), Mul(derivate(t1), Func("sin", t1)))
    case Func("tan", t1) => Mul(derivate(t1), Add(Num(1), Func("pow2", Func("tan", t1))))

    //Operator Derivation
    case Add(t1, t2) => Add(derivate(t1), derivate(t2))
    case Sub(t1, t2) => Sub(derivate(t1), derivate(t2))
    case Mul(t1, t2) => Add(Mul(derivate(t1), t2), Mul(t1, derivate(t2)))
    case Div(t1, t2) => Div(Sub(Mul(derivate(t1), t2), Mul(t1, derivate(t2))), Func("pow2", t2))

    //Var & Num Derivation
    case Num(t1) => Num(0)
    case Variable(t) => Num(1)
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

  lazy val funcRegex : Regex = """pow2|pow3|sqrt|abs|exp|ln|log|a?(?:sin|cos|tan)h?""".r
  lazy val funcExpr = "(" ~> expr <~ ")"

  lazy val func = funcRegex ~ opt(funcExpr) ^^ {
    case t1 ~ Some(t2) => Func(t1, t2)
  }
}