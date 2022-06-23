package Arithm

import org.sameersingh.scalaplot.{MemXYSeries, XYChart, XYData}
import org.sameersingh.scalaplot.jfreegraph.JFGraphPlotter
import org.jfree.chart.{JFreeChart => JChart}

object Plotter {

  def getXYChart(eq: String, points: (Seq[Double], Seq[Double])): XYChart ={
    val series = new XYData(new MemXYSeries(points._1, points._2, "f(x)"))
    val chart = new XYChart(eq, series)
    chart.showLegend = true
    chart
  }

  def getJFGraphPlotter(eq: String, points: (Seq[Double], Seq[Double])): JFGraphPlotter ={
    val chart = getXYChart(eq, points)
    val plotter = new JFGraphPlotter(chart)
    plotter
  }

  def getJChart(eq: String, points: (Seq[Double], Seq[Double])): JChart = {
    getJFGraphPlotter(eq, points).plotXYChart(getXYChart(eq, points))
  }

  def getDoubleXYChart(eq: String, points: (Seq[Double], Seq[Double]), points2: (Seq[Double], Seq[Double])): XYChart ={
    var series = new XYData(new MemXYSeries(points._1, points._2, "f(x)"))
    series += new MemXYSeries(points2._1, points2._2, "f'(x)")
    val chart = new XYChart(eq, series)
    chart.showLegend = true
    chart
  }

  def getDoubleJFGraphPlotter(eq: String, points: (Seq[Double], Seq[Double]), points2: (Seq[Double], Seq[Double])): JFGraphPlotter ={
    val chart = getDoubleXYChart(eq, points, points2)
    val plotter = new JFGraphPlotter(chart)
    plotter
  }

  def getDoubleJChart(eq: String, points: (Seq[Double], Seq[Double]), points2: (Seq[Double], Seq[Double])): JChart = {
    getJFGraphPlotter(eq, points).plotXYChart(getDoubleXYChart(eq, points, points2))
  }
}
