package GUI

import org.jfree.chart.{ChartPanel, JFreeChart => JChart}

import java.awt.GridLayout
import javax.swing._

object UI{
  def mainFrame(chart: JChart): Unit = {
    val frame = new JFrame("Arithmetic Graphical Calculator")
    val panel = new JPanel()

    panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS))
    panel.add(new ChartPanel(chart))
    panel.add(new JButton("ok"))


    frame.add(panel)
    frame.setSize(640, 420)
    //frame.add(new ChartPanel(chart))
    //frame.add(new JButton("Back"))
    frame.pack()
    frame.setVisible(true)
  }
}
