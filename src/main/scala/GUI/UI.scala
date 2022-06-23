package GUI

import Arithm.Plotter.{getDoubleJChart, getJChart}
import Arithm.Utils.{derivate, parseInput}
import Main.MainApp.eval_points
import org.jfree.chart.{ChartPanel, JFreeChart => JChart}

import java.awt.Dimension
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

object UI{
  def mainFrame(chart: JChart): Unit = {
    val frame = new JFrame("Arithmetic Graphical Calculator")
    val panel = new JPanel()
    val plotPanel = new JPanel()
    val buttonPanel = new JPanel()
    val textPanel = new JPanel()

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
        println("plotListener Button Clicked: ", textField.getText())
        val exp = parseInput(textField.getText())
        val points = eval_points(-10, 10, 0.1, exp)
        if (points == null) {
          println("Math Error")
        }
        else {
          val newPlot = getJChart(textField.getText(), points)
          plot = new ChartPanel(newPlot)
          plotPanel.removeAll()
          plotPanel.add(plot)
          plotPanel.updateUI()
        }
      }
    }

    class derivateListener extends ActionListener{
      override def actionPerformed(e: ActionEvent): Unit = {
        println("derivateListener Button Clicked", textField.getText())
        val exp = parseInput(textField.getText())
        val points = eval_points(-10, 10, 0.1, exp)
        val derivative = derivate(exp)
        val deriPoints = eval_points(-10, 10, 0.1, derivative)
        if (points == null || deriPoints == null){
          println("Math Error")
        }
        else{
          val newPlot = getDoubleJChart(textField.getText(), points, deriPoints)
          plot = new ChartPanel(newPlot)
          plotPanel.removeAll()
          plotPanel.add(plot)
          plotPanel.updateUI()
        }
      }
    }

    class simplifyListener extends ActionListener{
      override def actionPerformed(e: ActionEvent): Unit = {
        println("simplifyListener Button Clicked")
      }
    }


  }
}
