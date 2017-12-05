package patsyn


import java.awt.Dimension
import java.util.concurrent.TimeoutException

import measure.TimeTools
import patsyn.EvolutionRepresentation.{IndividualData, MemoryUsage}
import FuzzingTaskProvider.escapeStrings

object Runner {

  def main(args: Array[String]): Unit = {
    val ioId = if(args.isEmpty) 0 else args.head.toInt
    val workingDir = FileInteraction.getWorkingDir(ioId)

    runExample("phpHashNative", FuzzingTaskProvider.phpHashNativeExample(workingDir), RunConfig.default.withIoIdAndSeed(ioId, ioId))
  }

  case class MonitoringData(averageFitness: Double, bestFitness: Double, bestPerformance: Double)


  case class MonitorManager(monitorCallback: MonitoringData => Unit,
                            evalProgressCallback: Int => Unit,
                            saveMonitor: String => Unit)

  def createMonitor(populationSize: Int, ioId: Int,
                    width: Int = 600, height: Int = 450): MonitorManager = {
    import javax.swing._
    import visual._
    import org.jfree.chart.ChartUtilities
    import java.io.File

    val frame = new JFrame(s"GP Monitor [ioId=$ioId]") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setVisible(true)
    }

    var dataCollected = IS[MonitoringData]()

    def drawChart() = {
      import ListPlot.makeXY

      val avFitLine = dataCollected.map(_.averageFitness)
      val bestFitness = dataCollected.map(_.bestFitness)
      val bestPerformance = dataCollected.map(_.bestPerformance)

      ListPlot.plot(
        "best performance" -> makeXY(bestPerformance),
        "best fitness" -> makeXY(bestFitness),
        "average fitness" -> makeXY(avFitLine))("Performance Curve", "Generations", "Evaluation")
    }

    val progressLabel = new JLabel()
    val contentPane = new JPanel {
      add(progressLabel)
    }
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS))
    frame.setContentPane(contentPane)
    frame.setPreferredSize(new Dimension(width, height))
    frame.pack()

    MonitorManager(
      monitorCallback = d => {
        dataCollected :+= d

        val chart = drawChart()
        val monitorPanel = new MonitorPanel(chart, 10, (width, height))
        if (contentPane.getComponentCount > 1) {
          contentPane.remove(1)
        }
        contentPane.add(monitorPanel)
        frame.pack()
      },
      evalProgressCallback = p => {
        progressLabel.setText(s"progress: $p/$populationSize")
      },
      saveMonitor = name => {
        val chart = drawChart()
        ChartUtilities.saveChartAsPNG(new File(name), chart, width, height)
      }
    )
  }

  case class RunnerConfig(ioId: Int = 0,
                          randomSeed: Int = 0,
                          useGUI: Boolean = true,
                          keepBestIndividuals: Boolean = false,
                          previewPatternLen: Int = 7,
                          callExitAfterFinish: Boolean = true){
    def show: String = {
      s"""
         |ioId: $ioId
         |randomSeed: $randomSeed
         |previewPatternLen: $previewPatternLen
       """.stripMargin
    }
  }


  def run(problemConfig: ProblemConfig, gpEnv: GPEnvironment, config: RunConfig = RunConfig.default): Unit = {
    import problemConfig._
    import config._
    import config.gpConfig._
    import config.execConfig._
    import config.runnerConfig._

    val library = MultiStateGOpLibrary(gpEnv, outputTypes)
    val dateTimeString = TimeTools.numericalDateTime()
    val recordDirPath = {
      s"results-running/$problemName[ioId=$ioId,seed=$randomSeed]($dateTimeString)"
    }

    val (evalProgressCallback, monitorCallback, saveMonitor):
      (Int => Unit, MonitoringData => Unit, String => Unit) = {
      if (useGUI) {
        val monitor = createMonitor(populationSize, ioId)
        (monitor.evalProgressCallback, monitor.monitorCallback, monitor.saveMonitor)
      } else {
        val monitorDataPath = s"$recordDirPath/monitorData.txt"
        ((_: Int) => Unit, (data: MonitoringData) => {
          FileInteraction.writeToFile(monitorDataPath, append = true){
            s"${data.bestPerformance}, ${data.bestFitness}, ${data.averageFitness}\n"
          }
        }, (_: String) => Unit)
      }
    }

    FileInteraction.runWithAFileLogger(s"$recordDirPath/runLog.txt") { logger =>
      import logger._

      printSection("Configuration"){
        println(s"sizeOfInterest = $sizeOfInterest")
        println(gpEnv.show)
        println(config.show)
      }

      printSection("Function map"){
        gpEnv.functionMap.foreach { case (t, comps) =>
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
            resourceUsage(value)
          }
          , timeLimitInMillis.milliseconds
        )
      }

      val memoryLimit = sizeOfInterest * 4 * gpEnv.stateTypes.length
      val evaluation = new SimplePerformanceEvaluation(
        sizeOfInterest = sizeOfInterest, evaluationTrials = evaluationTrials, nonsenseFitness = -1.0,
        resourceUsage = timeLimitedResourceUsage(timeLimitInMillis), sizeF = sizeF, breakingMemoryUsage = memoryLimit
      )
      val representation = MultiStateRepresentation(
        totalSizeTolerance = totalSizeTolerance,
        singleSizeTolerance = singleSizeTolerance,
        stateTypes = gpEnv.stateTypes, outputTypes = outputTypes, evaluation = evaluation)
      val optimizer = EvolutionaryOptimizer(representation)
      val operators = IS(
        library.simpleCrossOp -> crossoverP,
        library.simpleMutateOp(newTreeMaxDepth = 3) -> mutateP,
        library.copyOp -> copyP
      )

      def showPattern(ind: MultiStateInd, memoryLimit: Long): String ={
        representation.individualToPattern(ind).takeWhile{
          case (MemoryUsage(mem), _) => mem < memoryLimit
        }.take(runnerConfig.previewPatternLen).toList.map {
          case (_, v) => escapeStrings(displayValue(v))
        }.mkString(", ")
      }

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
              representation.printIndividualMultiLine(println)(ind)
              println{
                showPattern(ind, memoryLimit)
              }
              FileInteraction.saveObjectToFile(s"$recordDirPath/timeoutIndividual.serialized")(ind)

              // We might also be interested in the value
              MultiStateRepresentation.saveExtrapolation(problemConfig ,ind, sizeOfInterest, memoryLimit,
                s"$recordDirPath/timeoutValue")

              System.exit(0)
              throw new Exception("Timed out!")
          }
        },
        threadNum = threadNum,
        randSeed = randomSeed,
        evalProgressCallback = evalProgressCallback
      )


      val startTime = System.nanoTime()

      var bestSoFar: Option[IndividualData[MultiStateInd]] = None
      var nonIncreasingTime = 0

      def setBestInd(indData: IndividualData[MultiStateInd]): Unit = {
        bestSoFar = Some(indData)

        val timeString = if (keepBestIndividuals) {
          val timeInNano = System.nanoTime() - startTime
          val timeInMillis = (timeInNano/1e6).toInt
          s"[time=$timeInMillis]"
        } else {
          ""
        }
        FileInteraction.saveObjectToFile(s"$recordDirPath/bestIndividual$timeString.serialized")(indData.ind)

        MultiStateRepresentation.saveExtrapolation(problemConfig, indData.ind,
          sizeOfInterest, memoryLimit, s"$recordDirPath/bestInput$timeString")
      }

      generations.takeWhile(pop => {
        nonIncreasingTime += 1
        bestSoFar match {
          case Some(previousBest) =>
            if (pop.bestIndData.evaluation.fitness > previousBest.evaluation.fitness) {
              nonIncreasingTime = 0
              setBestInd(pop.bestIndData)
            }
          case None => setBestInd(pop.bestIndData)
        }
        println(s"Last fitness increase: $nonIncreasingTime generations ago.")

        val shouldStop = {
          val timeInSec = (System.nanoTime() - startTime) / 1e9
          maxNonIncreaseGen.exists(nonIncreasingTime > _) ||
          maxFuzzingTimeSec.exists(timeInSec > _)
        }
        !shouldStop
      }).zipWithIndex.foreach { case (pop, i) =>
        val best = pop.bestIndData
        val data = MonitoringData(pop.averageFitness, best.evaluation.fitness, best.evaluation.performance)
        monitorCallback(data)
        saveMonitor(s"$recordDirPath/monitor.png")

        println("------------")
        print("[" + TimeTools.nanoToSecondString(System.nanoTime() - startTime) + "]")
        println(s"Generation ${i + 1}")
        println(s"Best Result: ${best.evaluation.showAsLinearExpr}, Created by ${best.history.birthOp}")
        representation.printIndividualMultiLine(println)(best.ind)
        println(s"Best Individual Pattern: ${showPattern(best.ind, memoryLimit)}, ...")
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

      println("Evolution Finished!")
      try{
        import java.io.File
        val performance = bestSoFar.get.evaluation.performance
        val newDir = s"results/$problemName[performance=$performance][ioId=$ioId,seed=$randomSeed]($dateTimeString)"
        FileInteraction.mkDirsAlongPath(newDir)
        new File(recordDirPath).renameTo(new File(newDir))
      }finally {
        if(callExitAfterFinish){
          System.exit(0)
        }
      }
    }
  }


  def runExample(taskName: String, taskProvider: FuzzingTaskProvider, config: RunConfig = RunConfig.default): Unit = {
    taskProvider.run{ task =>
      import task._
      import taskProvider._
      import config._

      val problemConfig = ProblemConfig(taskName, outputTypes = outputTypes, sizeF = sizeF, resourceUsage = resourceUsage,
        displayValue = displayValue,
        saveValueWithName = saveValueWithName)


      Runner.run(problemConfig, gpEnv, RunConfig(runnerConfig, gpConfig, execConfig))
    }

  }
}
