package singularity.visual

import java.awt.geom.Rectangle2D
import java.awt.{Dimension, Graphics, Graphics2D}
import java.text.{FieldPosition, NumberFormat, ParsePosition}
import javax.swing._

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import singularity._


object MonitorPanel{
//  def main(args: Array[String]): Unit = {
//    val frame = new JFrame("Monitor") {
//      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
//
//      setVisible(true)
//    }
//
//    val chart = {
//      import io.Source
//      val lines = Source.fromFile("/Users/weijiayi/Downloads/monitorData_random.txt").getLines()
//      plotFromCSVString(lines)
//    }
//
//    frame.setContentPane(new MonitorPanel(Some(chart), margin = 10, plotSize = (600,450)))
//    frame.pack()a
//  }

  def plotFromCSVString(lines: Iterator[String]): JFreeChart = {
    import ListPlot.makeXY

    val data = lines.map{ l => l.split(",\\s*").map(_.toDouble)}.toIndexedSeq
    val bestPerformance = data.map(_.apply(0))
    val bestFitness = data.map(_.apply(1))
    val avFitLine = data.map(_.apply(2))

    val chart = ListPlot.plot(
      "best performance" -> makeXY(bestPerformance),
      "best fitness" -> makeXY(bestFitness),
      "average fitness" -> makeXY(avFitLine))("Performance Curve", "Generations", "Evaluation")
    chart
  }
}

class MonitorPanel(var chart: Option[JFreeChart], margin: Double, plotSize: (Int, Int)) extends JPanel {
  setPreferredSize(new Dimension(plotSize._1, plotSize._2))

  override def paintComponent(g: Graphics) = {
    val g2d = g.asInstanceOf[Graphics2D]
    chart.foreach(_.draw(g2d, new Rectangle2D.Double(margin, margin, getWidth-2*margin, getHeight-2*margin)))
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

  def makeXY(ys: IS[Double]): IS[(Double, Double)] = {
    ys.indices.map { i => (i + 1).toDouble -> ys(i) }
  }
}