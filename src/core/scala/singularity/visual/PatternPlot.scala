package singularity.visual

import javax.swing.JFrame

import org.apache.commons.math3.fitting.PolynomialCurveFitter
import singularity.MultiStateRepresentation.individualToPattern
import singularity.StandardSystem.GraphValue
import singularity._

import scala.util.Random

object PatternPlot {
  def plotPatternPerformance(probConfig: ProblemConfig, individual: MultiStateInd,
                             sizeLimit: Int, maxPoints: Int = 10, memoryLimit: Long = Long.MaxValue,
                             patternCreationFeedback: (Int, IS[EValue]) => Unit,
                             seed: Int = 0): Stream[(Double, Double)] = {
    import EvolutionRepresentation.MemoryUsage

    var lastSize = Int.MinValue

    val values = individualToPattern(individual).takeWhile {
      case (MemoryUsage(memory), value) =>
        val newSize = probConfig.sizeF(value)
        patternCreationFeedback(newSize, value)
        if (newSize <= lastSize) {
          println("Warning: Can't reach specified size using this individual")
          false
        } else {
          lastSize = newSize
          newSize <= sizeLimit && memory <= memoryLimit
        }
    }.map(_._2).toIndexedSeq

    println(s"Values accumulated.")

    val valuesToUse = SimpleMath.randomSelectFrom(values, maxPoints, new Random(seed))


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
                             yLabel: String = "R",
                             exitOnClose: Boolean = true,
                             patternCreationFeedback: (Int, IS[EValue]) => Unit = (_,_) => (),
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

    val config: ProblemConfig = ???
    val sizeLimit = 1000
    val plotPoints = 30
    val files =
      """
        |/Users/weijiayi/Programming/PatternSyn/results-running/phphash_int_100[ioId=4,seed=4](18-02-20-14:23:19)/bestIndividual.serialized
      """.stripMargin.split("\n").map(_.trim).filter(_.nonEmpty)


    for(fileLine <- files) {
//      val lastName = "timeoutIndividual.serialized"
      val lastName = "bestIndividual.serialized"
      val file = if(fileLine.endsWith(".serialized")) fileLine else fileLine + "/" + lastName
      val ind = FileInteraction.readMultiIndFromFile(file, StandardSystem.funcMap)
      println("Individual: ")
      println(ind)


//      FuzzingTaskProvider.phpHashCollisionExample.runAsProbConfig("phpHash16") { config =>
        showResourceUsageChart(config, ind, sizeLimit,
          plotPoints = plotPoints, plotName = file, exitOnClose = false,
          memoryLimit = sizeLimit * ind.nStates * 4,
          patternCreationFeedback = (i, ev) => {
            if (i % 1 == 0) {
              println(s"input created: $i")
              ev match{
                case IS(graph: GraphValue, _, _) =>
                  println{
                    graph
                  }
                case _ => ()
              }
            }
          }
        )
//      }
    }
  }
}
