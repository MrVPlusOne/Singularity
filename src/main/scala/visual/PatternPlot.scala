package visual

import javax.swing.JFrame

import benchmarks.GuavaExamples
import patsyn.MultiStateRepresentation.individualToPattern
import patsyn._

import scala.util.Random

object PatternPlot {
  def plotPatternPerformance(probConfig: ProblemConfig, individual: MultiStateInd,
                             sizeLimit: Int, maxPoints: Int = 10, memoryLimit: Long = Long.MaxValue,
                             patternCreationFeedback: Int => Unit,
                             seed: Int = 0): Stream[(Double, Double)] = {
    import EvolutionRepresentation.MemoryUsage

    var lastSize = Int.MinValue

    val values = individualToPattern(individual).takeWhile {
      case (MemoryUsage(memory), value) =>
        val newSize = probConfig.sizeF(value)
        patternCreationFeedback(newSize)
        if (newSize <= lastSize) {
          println("Warning: Can't reach specified size using this individual")
          false
        } else {
          lastSize = newSize
          newSize <= sizeLimit && memory <= memoryLimit
        }
    }.map(_._2).toIndexedSeq

    println(s"Values accumulated.")

    val valuesToUse = randomSelectFrom(values, maxPoints, new Random(seed))


    valuesToUse.toStream.map(v => {
      val x = probConfig.sizeF(v).toDouble
      println(s"Evaluating at size $x")
      val y = probConfig.resourceUsage(v)
      (x, y)
    })
  }

  def randomSelectFrom[A](values: IS[A], maxPoints: Int, random: Random): IS[A] = {
    if (values.length <= maxPoints) {
      values
    } else {
      val xs = {
        var dataPointsLeft = maxPoints
        val filtered = values.indices.filter{i =>
          val keep = SimpleMath.randomGuess(random)(dataPointsLeft.toDouble/(values.length-i))
          if(keep){
            dataPointsLeft -= 1
          }
          keep
        }
        if (filtered.last != (values.length - 1)) filtered :+ (values.length - 1) else filtered
      }
      xs.map(values.apply)
    }
  }

  def showResourceUsageChart(probConfig: ProblemConfig,
                             ind: MultiStateInd,
                             sizeLimit: Int,
                             plotPoints: Int = 10,
                             lineName: String = "pattern",
                             plotName: String = "Resource Usage Chart",
                             xLabel: String = "size",
                             yLabel: String = "R",
                             exitOnClose: Boolean = true,
                             patternCreationFeedback: Int => Unit = _ => (),
                             memoryLimit: Long = Long.MaxValue
                            ): Unit
  = {
    val frame = new JFrame("Monitor") {
      if(exitOnClose){
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      }
      setVisible(true)
    }
    val xys = plotPatternPerformance(probConfig, ind, sizeLimit, maxPoints
      = plotPoints, memoryLimit, patternCreationFeedback = patternCreationFeedback)

    var data = IndexedSeq[(Double, Double)]()
    xys.foreach{ xy =>
      data :+= xy
      val chart = ListPlot.plot(lineName -> data)(plotName, xLabel, yLabel)
      frame.setContentPane(new MonitorPanel(Some(chart), margin = 10, plotSize = (600,450)))
      frame.pack()
    }
    println(data)

    println("Evaluation finished.")
  }

  def main(args: Array[String]): Unit = {
    import io.Source
    val lines = Source.fromFile(args.head).getLines()
    val sizeLimit = lines.next().toInt
    val plotPoints = lines.next().toInt
    val files = lines.toList


    for(fileLine <- files) {
      val lastName = "bestIndividual.serialized"
      val file = if(fileLine.endsWith(lastName)) fileLine else fileLine + "/" + lastName
      val ind = FileInteraction.readMultiIndFromFile(file, StandardSystem.funcMap)
      println("Individual: ")
      val config = GuavaExamples.immutableSet_copyOf
      showResourceUsageChart(config, ind, sizeLimit,
        plotPoints = plotPoints, plotName = file, exitOnClose = false,
        memoryLimit = sizeLimit * ind.nStates * 4,
        patternCreationFeedback = i => {
          if (i % 100 == 0) {
            println(s"input created: $i")
          }
        }
      )
    }
  }
}
