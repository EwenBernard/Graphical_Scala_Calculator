package Main

import ArithmParser.ExprParser
import ArithmParser.Utils.showExpr

object MainApp extends App with ExprParser {
  val eq = "2*x+ 2"
  val exp = parseAll(expr, eq).get
  val x = 2
  val step = 0.1
  /*
  println("Equation: ", eq)
  println("After Parsing: ", exp)
  println("Result= ", eval(exp, x), "with x = ", x)
  println("ShowExpr: ", showExpr(exp))*/
  println("Compute point: ", eval_points(0, 1, step, exp))
}

