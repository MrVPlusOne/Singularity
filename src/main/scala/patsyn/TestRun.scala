package patsyn


import java.awt.Dimension

import StandardSystem._
import measure.TimeTools
import patsyn.EvolutionRepresentation.{IndividualData, Population}

object TestRun {

  case class MonitorManager(monitorCallback: MonitoringData => Unit, evalProgressCallback: Int => Unit)

  def createMonitor(populationSize: Int): MonitorManager ={
    import javax.swing._
    import gui._

    val frame = new JFrame("GP Monitor") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setVisible(true)
    }

    var dataCollected = IS[MonitoringData]()
    var monitorPanel: MonitorPanel = null

    val progressLabel = new JLabel()
    val contentPane = new JPanel(){
      add(progressLabel)
    }
    frame.setContentPane(contentPane)
    frame.setPreferredSize(new Dimension(300,300))
    frame.pack()

    MonitorManager(
      monitorCallback = d => {
        dataCollected :+= d

        val avFitLine = dataCollected.map(_.averageFitness)
        val bestFitness = dataCollected.map(_.bestFitness)
        val bestPerformance = dataCollected.map(_.bestPerformance)

        val chart = ListPlot.plot(
          "best performance" -> makeXY(bestPerformance),
          "best fitness" -> makeXY(bestFitness),
          "average fitness" -> makeXY(avFitLine))("Performance Curve", "Generations", "Evaluation")
        monitorPanel = new MonitorPanel(chart, 10, (600, 450))
        if(contentPane.getComponentCount >= 1){
          contentPane.remove(1)
        }
        contentPane.add(monitorPanel)
        frame.pack()
      },
      evalProgressCallback = p => {
        progressLabel.setText(s"progress: $p/$populationSize")
      }
    )
  }

  def main(args: Array[String]): Unit = {
    runExample()
  }

  case class MonitoringData(averageFitness: Double, bestFitness: Double, bestPerformance: Double)

  def makeXY(ys: IS[Double]): IS[(Double, Double)] = {
    ys.indices.map{ i => (i+1).toDouble -> ys(i)}
  }


  def runExample(): Unit = {

//    val example = FuzzingExample.regexExample(
//      regex = "^(abc)+",  //"(\\.+\\|?)+",
//      regexDic = i => i.toChar.toString
//    )

    val example = FuzzingExample.graphAnalyzerExample

    val library = MultiStateGOpLibrary(example.gpEnv, example.outputTypes)

    println("[Function map]")
    example.gpEnv.functionMap.foreach { case (t, comps) =>
      println(s"$t -> ${comps.mkString("{", ", ", "}")}")
    }
    println("[End of Function map]")

    val recordDirPath = {
      import java.util.Calendar
      val dateTime = Calendar.getInstance().getTime
      s"results/$dateTime"
    }

    val populationSize = 1000
    val monitor = createMonitor(populationSize)
    import monitor.{evalProgressCallback, monitorCallback}

    for (seed <- 2 to 5) {
      val sizeOfInterest = example.sizeOfInterest
      val evaluation = new SimplePerformanceEvaluation(
        sizeOfInterest = sizeOfInterest, maxTrials = 3, nonsenseFitness = -1.0,
        resourceUsage = example.resourceUsage, sizeF = example.sizeF, maxMemoryUsage = sizeOfInterest*10
      )
      val representation = MultiStateRepresentation(totalSizeTolerance = 50, singleSizeTolerance = 30,
        stateTypes = example.gpEnv.stateTypes, outputTypes = example.outputTypes, evaluation = evaluation)
      val optimizer = EvolutionaryOptimizer(representation)
      val operators = IS(
        library.simpleCrossOp -> 0.4,
        library.simpleMutateOp(newTreeMaxDepth = 3) -> 0.5,
        library.copyOp -> 0.1
      )

      val generations = optimizer.optimize(
        populationSize = populationSize, tournamentSize = 7, neighbourSize = 490,
        initOperator = library.initOp(maxDepth = 3),
        operators = operators,
        indEval = ind => {
          representation.fitnessEvaluation(ind)._1
        },
        threadNum = 1,
        randSeed = seed,
        evalProgressCallback = evalProgressCallback
      )

      val maxNonIncreaseTime = 150

      FileInteraction.runWithAFileLogger(s"$recordDirPath/testResult[seed=$seed].txt") { logger =>
        import logger._

        val parameterInfo = ""
        println(parameterInfo)

        val startTime = System.nanoTime()

        var bestSoFar: Option[IndividualData[MultiStateInd]] = None
        var nonIncreasingTime = 0

        generations.takeWhile(pop => {
          val r = bestSoFar match {
            case Some(previousBest) =>
              if (pop.bestIndividual.evaluation.fitness > previousBest.evaluation.fitness) {
                nonIncreasingTime = 0
              }
              nonIncreasingTime <= maxNonIncreaseTime
            case None => true
          }
          bestSoFar = Some(pop.bestIndividual)
          nonIncreasingTime += 1
          r
        }).zipWithIndex.foreach { case (pop, i) =>
          val best = pop.bestIndividual
          val data = MonitoringData(pop.averageFitness, best.evaluation.fitness, best.evaluation.performance)
          monitorCallback(data)

          println("------------")
          print("[" + TimeTools.nanoToSecondString(System.nanoTime() - startTime) + "]")
          println(s"Generation ${i + 1}")
          println(s"Best Result: ${best.evaluation.showAsLinearExpr}, Created by ${best.history.birthOp}")
          representation.printIndividualMultiLine(println)(best.ind)
          val firstSevenInputs = representation.fitnessEvaluation(best.ind)._2.take(7).map{ example.displayValue
          }.mkString(", ")
          println(s"Best Individual Pattern: $firstSevenInputs, ...")
          println(s"Diversity: ${pop.fitnessMap.keySet.size}")
          println(s"Average Size: ${representation.populationAverageSize(pop)}")
          println(s"Average Fitness: ${pop.averageFitness}")
          println(s"Fitness Variation: ${pop.fitnessStdDiv}")
          print("Distribution: ")
          println {
            representation.frequencyRatioStat(pop.individuals.map(_.ind)).take(10).map {
              case (s, f) => s"$s -> ${"%.3f".format(f)}"
            }.mkString(", ")
          }
        }

        val data = bestSoFar.get
        FileInteraction.saveObjectToFile(s"$recordDirPath/bestIndividual[seed=$seed].serialized")(data)
      }
    }
  }

}
