package Main

import Arithm.ExprParser
import Arithm.Utils.showExpr
import GUI.UI
import Arithm.Plotter.{getJChart, getJFGraphPlotter}
import GUI.UI.mainFrame

import javax.swing.JFrame

object MainApp extends App with ExprParser {
  /*
  support - devant expression / var / func
  remplacer valeur dans zone de texte pour f'(x) et simplify
  Fix erreurs arithmétiques + fonctions.
  Valeur vide graph init
   */
  val eq = "0"
  val exp = parseInput(eq)
  val points = eval_points(0, 1, 1, exp)
  /*
  println("Equation: ", eq)
  println("After Parsing: ", exp)
  println("Result= ", eval(exp, x), "with x = ", x)
  println("ShowExpr: ", showExpr(exp))
  println("Compute point: ", eval_points(0, 1, step, exp))*/

  mainFrame(getJChart("0", points))
}

