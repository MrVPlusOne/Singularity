package gui

import java.awt.geom.Rectangle2D
import java.awt.{Dimension, Graphics, Graphics2D}
import java.text.{DecimalFormat, FieldPosition, NumberFormat, ParsePosition}
import javax.swing._

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import patsyn._


object MonitorPanel{
  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Monitor") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

      setVisible(true)
    }

    for(i <- 0 until 12){
      val chart = ListPlot.plot(s"x^$i" -> (1 to 10).map(x => x.toDouble -> math.pow(x, i)))("Demo", "x", "y")
      frame.setContentPane(new MonitorPanel(chart, 10, (600,450)))
      frame.pack()
      Thread.sleep(1)
    }
  }
}

class MonitorPanel(chart: JFreeChart, margin: Double, plotSize: (Int, Int)) extends JPanel {
  setPreferredSize(new Dimension(plotSize._1, plotSize._2))

  override def paintComponent(g: Graphics) = {
    val g2d = g.asInstanceOf[Graphics2D]
    chart.draw(g2d, new Rectangle2D.Double(margin, margin, getWidth-2*margin, getHeight-2*margin))
  }
}

object ListPlot {
  def plot(lines: (String, IS[(Double, Double)])*)(plotName: String, xLabel: String, yLabel: String, orientation: PlotOrientation = PlotOrientation.VERTICAL): JFreeChart = {
    val dataSet = new XYSeriesCollection()
    lines.foreach{ case (lineName, xys) =>
      val series = new XYSeries(lineName)
      xys.foreach{ case (x,y) =>
        series.add(x,y)
      }
      dataSet.addSeries(series)
    }

    val chart = ChartFactory.createXYLineChart(plotName, xLabel, yLabel, dataSet,
      orientation, true, true, false
    )
    val rangeAxis = chart.getXYPlot.getRangeAxis().asInstanceOf[NumberAxis]
    rangeAxis.setNumberFormatOverride(new NumberFormat {
      def format(number: Double, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer = {
        toAppendTo.append("%.3g".format(number))
        toAppendTo
      }
      def format(number: Long, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer = throw new Exception()
      def parse(source: String, parsePosition: ParsePosition): Number = throw new Exception()
    })

    chart
  }
}