package visual

import org.jfree.chart.JFreeChart
import patsyn.MultiStateRepresentation.individualToPattern
import patsyn.{EvolutionRepresentation, FileInteraction, FuzzingTaskProvider, MultiStateInd}

import scala.util.Random

object PatternPlot {
  def plotPatternPerformance(taskProvider: FuzzingTaskProvider, individual: MultiStateInd,
                             sizeLimit: Int, patternName: String, maxPoints: Int = 10, memoryLimit: Long = Long.MaxValue, seed: Int = 0): JFreeChart = {
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
      val line = valuesToUse.map(v => {
        val x = taskProvider.sizeF(v).toDouble
        println(s"Evaluating at size $x")
        val y = task.resourceUsage(v)
        (x, y)
      })


      ListPlot.plot(patternName -> line)("Resource Usage Chart", xLabel = "size", yLabel = "R")
    }
  }

  def main(args: Array[String]): Unit = {
    val ind = FileInteraction.readObjectFromFile[MultiStateInd]("someIndividual.serialized")
    plotPatternPerformance(???, ind, sizeLimit = ???, patternName = ???)
  }
}
