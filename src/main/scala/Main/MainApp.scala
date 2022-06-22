package Main

import Arithm.ExprParser
import Arithm.Utils.showExpr
import GUI.UI
import Arithm.Plotter.getJFGraphPlotter
import org.sameersingh.scalaplot.jfreegraph.JFGraphPlotter
import org.sameersingh.scalaplot.{MemXYSeries, XYChart, XYData}

object MainApp extends App with ExprParser {
  val eq = "2*x+ 2"
  val exp = parseAll(expr, eq).get
  val x = 2
  val step = 0.1
  val points = eval_points(0, 1, step, exp)
  /*
  println("Equation: ", eq)
  println("After Parsing: ", exp)
  println("Result= ", eval(exp, x), "with x = ", x)
  println("ShowExpr: ", showExpr(exp))*/
  //println("Compute point: ", eval_points(0, 1, step, exp))
  //val ui = new UI
  //ui.visible = true
  getJFGraphPlotter(eq, points).gui()
}

