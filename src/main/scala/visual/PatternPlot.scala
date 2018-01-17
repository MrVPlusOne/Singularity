package visual

import javax.swing.JFrame

import benchmarks.AccountingWizardExample
import patsyn.MultiStateRepresentation.individualToPattern
import patsyn.StandardSystem.{GraphValue, VectValue}
import patsyn._

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
        val filtered = values.indices.filter { i =>
          val keep = SimpleMath.randomGuess(random)(dataPointsLeft.toDouble / (values.length - i))
          if (keep) {
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
                             patternCreationFeedback: (Int, IS[EValue]) => Unit = (_, _) => (),
                             memoryLimit: Long = Long.MaxValue
                            ): Unit
  = {
    val frame = new JFrame("Monitor") {
      if (exitOnClose) {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      }
      setVisible(true)
    }
    val xys = plotPatternPerformance(probConfig, ind, sizeLimit, maxPoints
      = plotPoints, memoryLimit, patternCreationFeedback = patternCreationFeedback)

    var data = IndexedSeq[(Double, Double)]()
    xys.foreach { xy =>
      data :+= xy
      val chart = ListPlot.plot(lineName -> data)(plotName, xLabel, yLabel)
      frame.setContentPane(new MonitorPanel(Some(chart), margin = 10, plotSize = (600, 450)))
      frame.pack()
    }
    println(data)

    println("Evaluation finished.")
  }

  def timeoutTest(): Unit ={
    implicit val classPath = FileInteraction.getClassPath(inIDE = true)
    val config = AccountingWizardExample.fileSizeExample(ioId = 1)


    import StandardSystem._
    val timeoutHead = FileInteraction.readObjectFromFile[EValue]("results/stac.e5.accountingwizard.filesize[performance=timeout][ioId=0,seed=0](18-01-05-17:00:53)/timeoutValue.serialized")
    val timeoutValue = IS[EValue](timeoutHead, VectValue(Vector(5,1,8,8,2,1,9,3,9,2,0)), 0)

    for(i <- 0 until 100){
      println(s"timeout value usage [$i]:")
      config.resourceUsage(timeoutValue)
    }

  }

  def testAccountingWizard(): Unit ={
    import StandardSystem._

    implicit val classPath = FileInteraction.getClassPath(inIDE = true)
    val config = AccountingWizardExample.fileSizeExample(ioId = 1)
    val sizeLimit = 10000
    val plotPoints = 50
    val file =
      """
        |results/seed101/bestInput.serialized
      """.stripMargin.split("\n").map(_.trim).filter(_.nonEmpty).head

    val inputValue = FileInteraction.readObjectFromFile[Vector[EValue]](file)
    println("Individual: ")

    val Vector(orders: VectValue, name, budget) = inputValue

    val nameContent = name.asInstanceOf[VectValue].value
    val largerInput = IS(VectValue(Vector.fill(6)(orders.value).flatten),
      VectValue(Vector.fill(600)(IntValue(-316))++nameContent), budget)
//    Debug.log("name length")(name.asInstanceOf[VectValue].value.length)
    println{
      config.sizeF(largerInput)
    }
    println{
      config.resourceUsage(largerInput)
    }


  }

  def main(args: Array[String]): Unit = {
//    timeoutTest()
    testAccountingWizard()
    return

    implicit val classPath = FileInteraction.getClassPath(inIDE = true)
    val config = AccountingWizardExample.fileSizeExample(ioId = 1)
    val sizeLimit = 10000
    val plotPoints = 50
    val files =
      """
        |results/seed101/bestIndividual.serialized
      """.stripMargin.split("\n").map(_.trim).filter(_.nonEmpty)

//    import StandardSystem._
//    val timeoutValue = FileInteraction.readObjectFromFile[Vector[EValue]]("results/stac.e5.accountingwizard.filesize[performance=timeout][ioId=1,seed=1](18-01-06-16:17:26)/timeoutValue.serialized")
//
//    Debug.log("timeout value usage"){
//      config.resourceUsage(timeoutValue)
//    }


    for (fileLine <- files) {
      val lastName = "timeoutIndividual.serialized"
//      val lastName = "bestIndividual.serialized"
      val file = if (fileLine.endsWith(".serialized")) fileLine else fileLine + "/" + lastName
      val ind = FileInteraction.readMultiIndFromFile(file, StandardSystem.funcMap)
      println("Individual: ")

      showResourceUsageChart(config, ind, sizeLimit,
        plotPoints = plotPoints, plotName = file, exitOnClose = false,
        memoryLimit = sizeLimit * ind.nStates * 4,
        patternCreationFeedback = (i, ev) => {
          if (i % 1 == 0) {
            println(s"input created: $i")
            ev match {
              case IS(graph: GraphValue, _, _) =>
                println {
                  MamFormat.showAsMamGraph(graph)
                }
              case _ =>
            }
          }
        }
      )
    }
  }
}
