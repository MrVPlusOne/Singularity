package patsyn


import java.awt.Dimension
import java.util.concurrent.TimeoutException

import measure.TimeTools
import patsyn.EvolutionRepresentation.IndividualData
import FuzzingTaskProvider.escapeStrings
import cli.BenchmarkDriver

object Runner {

  case class MonitorManager(monitorCallback: MonitoringData => Unit, evalProgressCallback: Int => Unit)

  def createMonitor(populationSize: Int): MonitorManager = {
    import javax.swing._

    import gui._

    val frame = new JFrame("GP Monitor") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setVisible(true)
    }

    var dataCollected = IS[MonitoringData]()
    var monitorPanel: MonitorPanel = null

    val progressLabel = new JLabel()
    val contentPane = new JPanel {
      add(progressLabel)
    }
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS))
    frame.setContentPane(contentPane)
    frame.setPreferredSize(new Dimension(600, 480))
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
        if (contentPane.getComponentCount > 1) {
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
    val ioId = if(args.isEmpty) 0 else args.head.toInt
    val workingDir = s"workingDir$ioId"
    FileInteraction.mkDirsAlongPath(workingDir)

    runExample(FuzzingTaskProvider.airplan5Example(workingDir), Seq(ioId), useGUI = true, config = RunConfig.default.copy(timeLimitInMillis = 120000))
  }

  case class MonitoringData(averageFitness: Double, bestFitness: Double, bestPerformance: Double)

  def makeXY(ys: IS[Double]): IS[(Double, Double)] = {
    ys.indices.map { i => (i + 1).toDouble -> ys(i) }
  }

  case class RunConfig(populationSize: Int = 500,
                       tournamentSize: Int = 7,
                       evaluationTrials: Int = 1,
                       totalSizeTolerance: Int = 60,
                       singleSizeTolerance: Int = 30,
                       threadNum: Int = 1,
                       timeLimitInMillis: Int = 20000,
                       maxNonIncreaseTime: Int = 150
                      ){
    def show: String = {
      s"""
         |populationSize: $populationSize
         |tournamentSize: $tournamentSize
         |evaluationTrials：$evaluationTrials
         |totalSizeTolerance：$totalSizeTolerance
         |singleSizeTolerance：$singleSizeTolerance
         |threadNum：$threadNum
         |timeLimitInMillis：$timeLimitInMillis
         |maxNonIncreaseTime：$maxNonIncreaseTime
       """.stripMargin
    }
  }

  object RunConfig{
    def default = RunConfig()
  }

  def runExample(taskProvider: FuzzingTaskProvider, seeds: Seq[Int], config: RunConfig = RunConfig.default,
                 useGUI: Boolean = true)
  : Unit = taskProvider
    .run{ task =>

    import config._

    val library = MultiStateGOpLibrary(task.gpEnv, task.outputTypes)

    for (seed <- seeds) {
      val recordDirPath = {
        import java.util.Calendar
        val dateTime = Calendar.getInstance().getTime
        s"results/$dateTime[seed=$seed]"
      }

      val (evalProgressCallback, monitorCallback): (Int => Unit, MonitoringData => Unit) = {
        if (useGUI) {
          val monitor = createMonitor(populationSize)
          (monitor.evalProgressCallback, monitor.monitorCallback)
        } else {
          val monitorDataPath = s"$recordDirPath/monitorData.txt"
          ((_: Int) => Unit, (data: MonitoringData) => {
            FileInteraction.writeToFile(monitorDataPath, append = true){
              s"${data.bestPerformance}, ${data.bestFitness}, ${data.averageFitness}\n"
            }
          })
        }
      }

      FileInteraction.runWithAFileLogger(s"$recordDirPath/testResult[seed=$seed].txt") { logger =>
        import logger._

        def printSection[A](name: String)(content: => A): A = {
          println(s"[$name]")
          val r = content
          println(s"[End of $name]\n")
          r
        }

        printSection("Configuration"){
          println(s"sizeOfInterest = ${task.sizeOfInterest}")
          println(config.show)
        }

        printSection("Function map"){
          task.gpEnv.functionMap.foreach { case (t, comps) =>
            println(s"$t -> ${comps.mkString("{", ", ", "}")}")
          }
        }

        @throws[TimeoutException]
        def timeLimitedResourceUsage(timeLimitInMillis: Int)(value: IS[EValue]): Double = {
          import scala.concurrent.ExecutionContext.Implicits.global
          import scala.concurrent._
          import scala.concurrent.duration._
          Await.result(
            Future {
              task.resourceUsage(value)
            }
            , timeLimitInMillis.milliseconds
          )
        }

        val sizeOfInterest = task.sizeOfInterest
        val evaluation = new SimplePerformanceEvaluation(
          sizeOfInterest = sizeOfInterest, evaluationTrials = evaluationTrials, nonsenseFitness = -1.0,
          resourceUsage = timeLimitedResourceUsage(timeLimitInMillis), sizeF = taskProvider.sizeF, breakingMemoryUsage = sizeOfInterest * 10
        )
        val representation = MultiStateRepresentation(totalSizeTolerance = totalSizeTolerance,
          singleSizeTolerance = singleSizeTolerance,
          stateTypes = task.gpEnv.stateTypes, outputTypes = task.outputTypes, evaluation = evaluation)
        val optimizer = EvolutionaryOptimizer(representation)
        val operators = IS(
          library.simpleCrossOp -> 0.4,
          library.simpleMutateOp(newTreeMaxDepth = 3) -> 0.5,
          library.copyOp -> 0.1
        )

        val generations = optimizer.optimize(
          populationSize = populationSize, tournamentSize = tournamentSize,
          initOperator = library.initOp(maxDepth = 3),
          operators = operators,
          indEval = ind => {
            try {
              representation.fitnessEvaluation(ind)._1
            } catch {
              case _: TimeoutException =>
                println("Evaluation timed out!")
                val firstSevenInputs = representation.individualToPattern(ind).take(7).toList.map {
                  case (_, v) => escapeStrings(taskProvider.displayValue(v))
                }.mkString(", ")
                representation.printIndividualMultiLine(println)(ind)
                println(s"Individual Pattern: $firstSevenInputs, ...")
                FileInteraction.saveObjectToFile(s"$recordDirPath/timeoutIndividual[seed=$seed].serialized")(ind)

                // We might also be interested in the value
                BenchmarkDriver.saveExtrapolation(taskProvider, ind, task.sizeOfInterest, Long.MaxValue,
                  s"$recordDirPath/timeoutValue")

                System.exit(0)
                throw new Exception("Timed out!")
            }
          },
          threadNum = threadNum,
          randSeed = seed,
          evalProgressCallback = evalProgressCallback
        )


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
          val firstSevenInputs = representation.individualToPattern(best.ind).take(7).toList.map {
            case (_, v) => escapeStrings(taskProvider.displayValue(v))
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
          FileInteraction.saveObjectToFile(s"$recordDirPath/bestIndividual[seed=$seed].serialized")(best.ind)
        }

        println("Evolution Finished!")
        System.exit(0)
      }
    }
  }
}