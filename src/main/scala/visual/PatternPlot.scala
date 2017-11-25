package visual

import java.awt.Dimension
import javax.swing.JFrame

import org.jfree.chart.JFreeChart
import patsyn.MultiStateRepresentation.individualToPattern
import patsyn.{EvolutionRepresentation, FileInteraction, FuzzingTaskProvider, MultiStateInd}

import scala.util.Random

object PatternPlot {
  def plotPatternPerformance(taskProvider: FuzzingTaskProvider, individual: MultiStateInd,
                             sizeLimit: Int, maxPoints: Int = 10, memoryLimit: Long = Long.MaxValue, seed: Int = 0): Stream[(Double, Double)] = {
    import EvolutionRepresentation.MemoryUsage

    var lastSize = Int.MinValue

    val values = individualToPattern(individual).takeWhile{
      case (MemoryUsage(memory), value) =>
        val newSize = taskProvider.sizeF(value)
        if(newSize <= lastSize){
          println("Warning: Can't reach specified size using this individual")
          false
        }else{
          lastSize = newSize
          newSize <= sizeLimit && memory <= memoryLimit
        }
    }.map(_._2).toIndexedSeq

    println(s"Values accumulated.")

    val valuesToUse = if(values.length <= maxPoints){
      values
    } else {
      val r = new Random(seed)
      val p = maxPoints.toDouble / values.length
      val xs = {
        val filtered = values.indices.filter(_ => r.nextDouble()<=p)
        if(filtered.last != (values.length-1)) filtered :+ (values.length-1) else filtered
      }
      xs.map(values.apply)
    }

    taskProvider.run { task =>
      valuesToUse.toStream.map(v => {
        val x = taskProvider.sizeF(v).toDouble
        println(s"Evaluating at size $x")
        val y = task.resourceUsage(v)
        (x, y)
      })
    }
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Monitor") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setVisible(true)
    }

    val workDir = FileInteraction.getWorkingDir(0)
    val ind = FileInteraction.readObjectFromFile[MultiStateInd]("results/17-11-24-18:24:27[ioId=2]/bestIndividual[seed=2].serialized")
    val xys = plotPatternPerformance(FuzzingTaskProvider.fordFulkersonExample(false), ind, sizeLimit = 20000)

    var data = IndexedSeq[(Double, Double)]()
    xys.foreach{ xy =>
      data :+= xy
      val chart = ListPlot.plot("pattern[seed=1]" -> data)("Resource Usage Chart", xLabel = "size", yLabel = "R")
      frame.setContentPane(new MonitorPanel(chart, margin = 10, plotSize = (600,450)))
      frame.pack()
    }

    println("Evaluation finished.")
  }
}
