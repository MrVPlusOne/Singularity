package visual

import java.awt.Dimension
import javax.swing.JFrame

import benchmarks.GuavaExamples
import org.jfree.chart.JFreeChart
import patsyn.MultiStateRepresentation.individualToPattern
import patsyn._

import scala.util.Random

object PatternPlot {
  def plotPatternPerformance(probConfig: ProblemConfig, individual: MultiStateInd,
                             sizeLimit: Int, maxPoints: Int = 10, memoryLimit: Long = Long.MaxValue, seed: Int = 0): Stream[(Double, Double)] = {
    import EvolutionRepresentation.MemoryUsage

    var lastSize = Int.MinValue

    val values = individualToPattern(individual).takeWhile {
      case (MemoryUsage(memory), value) =>
        val newSize = probConfig.sizeF(value)
        if (newSize <= lastSize) {
          println("Warning: Can't reach specified size using this individual")
          false
        } else {
          lastSize = newSize
          newSize <= sizeLimit && memory <= memoryLimit
        }
    }.map(_._2).toIndexedSeq

    println(s"Values accumulated.")

    val valuesToUse = if (values.length <= maxPoints) {
      values
    } else {
      val r = new Random(seed)
      val xs = {
        var dataPointsLeft = maxPoints
        val filtered = values.indices.filter{i =>
          val keep = SimpleMath.randomGuess(r)(dataPointsLeft.toDouble/(values.length-i))
          if(keep){
            dataPointsLeft -= 1
          }
          keep
        }
        if (filtered.last != (values.length - 1)) filtered :+ (values.length - 1) else filtered
      }
      xs.map(values.apply)
    }


    valuesToUse.toStream.map(v => {
      val x = probConfig.sizeF(v).toDouble
      println(s"Evaluating at size $x")
      val y = probConfig.resourceUsage(v)
      (x, y)
    })
  }

  def showResourceUsageChart(probConfig: ProblemConfig,
                             ind: MultiStateInd,
                             sizeLimit: Int,
                             plotPoints: Int = 10,
                             lineName: String = "pattern",
                             plotName: String = "Resource Usage Chart",
                             xLabel: String = "size",
                             yLabel: String = "R"): Unit
  = {
    val frame = new JFrame("Monitor") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setVisible(true)
    }
    val xys = plotPatternPerformance(probConfig, ind, sizeLimit, maxPoints
      = plotPoints)

    var data = IndexedSeq[(Double, Double)]()
    xys.foreach{ xy =>
      data :+= xy
      val chart = ListPlot.plot(lineName -> data)(plotName, xLabel, yLabel)
      frame.setContentPane(new MonitorPanel(chart, margin = 10, plotSize = (600,450)))
      frame.pack()
    }
    println(data)

    println("Evaluation finished.")
  }

  def main(args: Array[String]): Unit = {
    val patternFile = "results-running/check[performance~=6e5,ioId=4,seed=4](17-12-03-16:28:54)/bestIndividual.serialized"
    val sizeLimit = 500
    val ind = FileInteraction.readObjectFromFile[MultiStateInd](patternFile)
    val config = GuavaExamples.immutableBiMap_inverse
    showResourceUsageChart(config, ind, sizeLimit, plotPoints = 30)
  }
}
