package GUI

import Arithm.Plotter.{getDoubleJChart, getJChart}
import Arithm.Utils.{derivate, parseInput, showExpr}
import Main.MainApp.eval_points
import org.jfree.chart.{ChartPanel, JFreeChart => JChart}

import java.awt.Dimension
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

object UI{
  def mainFrame(chart: JChart): Unit = {

    val digitPattern = "^-?[0-9]\\d*(\\.\\d+)?$".r
    var Xlim1 = 0.0
    var Xlim2 = 40.0
    var step = 0.1

    val frame = new JFrame("Arithmetic Graphical Calculator")
    val panel = new JPanel()
    val plotPanel = new JPanel()
    val buttonPanel = new JPanel()
    val textPanel = new JPanel()
    val xWindowPanel = new JPanel()

    var plot = new ChartPanel(chart)

    val textField = new JTextField()
    textPanel.add(new JLabel("f(x) = "))
    textPanel.add(textField)


    textField.setPreferredSize(new Dimension( 400, 24 ))
    textField.setMaximumSize(textField.getPreferredSize)

    panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS))
    plotPanel.add(plot)
    panel.add(plotPanel)
    panel.add(textPanel)

    val xWindowTextField1 = new JTextField()
    xWindowTextField1.setPreferredSize(new Dimension( 80, 24 ))
    xWindowTextField1.setMaximumSize(textField.getPreferredSize)
    val xWindowTextField2 = new JTextField()
    xWindowTextField2.setPreferredSize(new Dimension( 80, 24 ))
    xWindowTextField2.setMaximumSize(textField.getPreferredSize)
    val xStep = new JTextField()
    xStep.setPreferredSize(new Dimension( 80, 24 ))
    xStep.setMaximumSize(textField.getPreferredSize)
    xWindowPanel.add(new JLabel("-Xlim = "))
    xWindowPanel.add(xWindowTextField1)
    xWindowPanel.add(new JLabel("+Xlim = "))
    xWindowPanel.add(xWindowTextField2)
    xWindowPanel.add(new JLabel(" XStep = "))
    xWindowPanel.add(xStep)

    panel.add(xWindowPanel)

    val plotBtn = new JButton("Plot Graph")
    val derivateBtn = new JButton("Derivate")
    val simplifyBtn = new JButton("Simplify")

    plotBtn.addActionListener(new plotListener())
    derivateBtn.addActionListener(new derivateListener())
    simplifyBtn.addActionListener(new simplifyListener())

    buttonPanel.add(plotBtn)
    buttonPanel.add(derivateBtn)
    buttonPanel.add(simplifyBtn)

    panel.add(buttonPanel)

    frame.add(panel)
    frame.setSize(640, 420)
    frame.pack()
    frame.setVisible(true)

    class plotListener extends ActionListener{
      override def actionPerformed(e: ActionEvent): Unit = {

        val exp = parseInput(textField.getText())
        val XlimText1 = xWindowTextField1.getText()
        val XlimText2 = xWindowTextField2.getText()
        val stepText = xStep.getText()

        if (digitPattern.findFirstIn(XlimText1).exists(_.trim.nonEmpty)){
          Xlim1 = digitPattern.findFirstIn(XlimText1).get.toDouble
          print(Xlim1)
        }
        if (digitPattern.findFirstIn(XlimText2).exists(_.trim.nonEmpty)){
          Xlim2 = digitPattern.findFirstIn(XlimText2).get.toDouble
          print(Xlim2)
        }
        if (digitPattern.findFirstIn(stepText).exists(_.trim.nonEmpty)){
          step = digitPattern.findFirstIn(stepText).get.toDouble
          print(step)
        }

        val points = eval_points(Xlim1, Xlim2, step, exp)
        val newPlot = getJChart(textField.getText(), points)
        plot = new ChartPanel(newPlot)
        plotPanel.removeAll()
        plotPanel.add(plot)
        plotPanel.updateUI()
      }
    }

    class derivateListener extends ActionListener{
      override def actionPerformed(e: ActionEvent): Unit = {

        val exp = parseInput(textField.getText())
        val XlimText1 = xWindowTextField1.getText()
        val XlimText2 = xWindowTextField2.getText()
        val stepText = xStep.getText()

        if (digitPattern.findFirstIn(XlimText1).exists(_.trim.nonEmpty)){
          Xlim1 = digitPattern.findFirstIn(XlimText1).get.toDouble
          print(Xlim1)
        }
        if (digitPattern.findFirstIn(XlimText2).exists(_.trim.nonEmpty)){
          Xlim2 = digitPattern.findFirstIn(XlimText2).get.toDouble
          print(Xlim2)
        }
        if (digitPattern.findFirstIn(stepText).exists(_.trim.nonEmpty)){
          step = digitPattern.findFirstIn(stepText).get.toDouble
          print(step)
        }

        val points = eval_points(Xlim1, Xlim2, step, exp)
        val derivative = derivate(exp)
        val deriPoints = eval_points(Xlim1, Xlim2, step, derivative)
        val newPlot = getDoubleJChart("f(x) = " + textField.getText() + "\n f'(x) = " + showExpr(derivative), points, deriPoints)
        plot = new ChartPanel(newPlot)
        plotPanel.removeAll()
        plotPanel.add(plot)
        plotPanel.updateUI()
      }
    }

    class simplifyListener extends ActionListener{
      override def actionPerformed(e: ActionEvent): Unit = {
        println("simplifyListener Button Clicked")
      }
    }


  }
}
