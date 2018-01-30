package patsyn


import java.awt.Dimension
import java.util.concurrent.TimeoutException

import measure.TimeTools
import patsyn.EvolutionRepresentation.{IndividualData, MemoryUsage}
import FuzzingTaskProvider.escapeStrings

import scala.util.Random

object Runner {

  def main(args: Array[String]): Unit = {
    val ioId = if(args.isEmpty) 0 else args.head.toInt
    val workingDir = FileInteraction.getWorkingDir(ioId)

    runExample("phpHash", FuzzingTaskProvider.phpHashCollisionExample, RunConfig.default.withIoIdAndSeed(ioId, ioId))
  }

  case class MonitoringData(averageFitness: Double, bestFitness: Double, bestPerformance: Double)


  case class MonitorManager(monitorCallback: MonitoringData => Unit,
                            evalProgressCallback: String => Unit,
                            saveMonitor: String => Unit)

  def createMonitor(barTitle: String, ioId: Int,
                    width: Int = 600, height: Int = 450): MonitorManager = {
    import javax.swing._
    import visual._
    import org.jfree.chart.ChartUtilities
    import java.io.File

    val frame = new JFrame(barTitle) {
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

    val progressLabel = new JLabel("Initialize...")
    val monitorPanel = new MonitorPanel(None, 10, (width, height))
    val contentPane = new JPanel()
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS))
    contentPane.add(progressLabel)
    contentPane.add(monitorPanel)
    contentPane.setPreferredSize(new Dimension(width, height))
    frame.setContentPane(contentPane)
    frame.pack()



    MonitorManager(
      monitorCallback = d => {
        dataCollected :+= d
        val chart = drawChart()
        monitorPanel.chart = Some(chart)
        monitorPanel.repaint()
      },
      evalProgressCallback = s => {
        progressLabel.setText(s)
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
                          callExitAfterFinish: Boolean = true
                         ){
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

    case class ReEvaluatedPop(originalData: IS[IndividualData[MultiStateInd]],
                              refEvaluations: IS[IndividualData[MultiStateInd]]){
      val populationSize: Int = refEvaluations.length

      lazy val averageFitness: Double = {
        refEvaluations.map(_.evaluation.fitness).sum/populationSize
      }

      lazy val fitnessStdDiv: Double = {
        val aveFit = averageFitness
        math.sqrt{
          refEvaluations.map(e => SimpleMath.square(e.evaluation.fitness - aveFit)).sum / populationSize
        }
      }

      lazy val averagePerformance: Double = {
        refEvaluations.map(_.evaluation.performance).sum/populationSize
      }

      def bestInd: IndividualData[MultiStateInd] = {
        refEvaluations.maxBy(_.evaluation.fitness)
      }
    }

    val rand = new Random(randomSeed)

    val library = MultiStateGOpLibrary(gpEnv, outputTypes)
    val dateTimeString = TimeTools.numericalDateTime()
    val recordDirPath = {
      s"results-running/$problemName[ioId=$ioId,seed=$randomSeed]($dateTimeString)"
    }
    def renameResultDir(perf: String) = {
      import java.io.File

      val newDir = s"results/$problemName[performance=$perf][ioId=$ioId,seed=$randomSeed]($dateTimeString)"
      FileInteraction.mkDirsAlongPath(newDir)
      new File(recordDirPath).renameTo(new File(newDir))
    }

    val (evalProgressCallback, monitorCallback, saveMonitor):
      (String => Unit, MonitoringData => Unit, String => Unit) = {
      if (useGUI) {
        val monitor = createMonitor(s"$problemName[ioId=$ioId, popSize=$populationSize]", ioId)
        (monitor.evalProgressCallback, monitor.monitorCallback, monitor.saveMonitor)
      } else {
        val monitorDataPath = s"$recordDirPath/monitorData.txt"
        ((_: String) => Unit, (data: MonitoringData) => {
          FileInteraction.writeToFile(monitorDataPath, append = true){
            s"${data.bestPerformance}, ${data.bestFitness}, ${data.averageFitness}\n"
          }
        }, (_: String) => Unit)
      }
    }

    @throws[TimeoutException]
    def timeLimitedResourceUsage(timeLimitInMillis: Int)(value: IS[EValue]): Double = {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent._
      import scala.concurrent.duration._
      Await.result(
        Future (resourceUsage(value)), timeLimitInMillis.milliseconds)
    }

    def calcMemoryLimit(sizeOfInterest: Int): Long = {
      sizeOfInterest * 4 * gpEnv.stateTypes.length
    }

    def newEvalSize(): (Int, MemoryUsage) = {
      val sizeOfInterest = evalSizePolicy match {
        case FixedEvalSize(s) => s
        case p: VariedEvalSize =>
          p.f(rand)
      }
      val memoryLimit = calcMemoryLimit(sizeOfInterest)
      (sizeOfInterest, MemoryUsage(memoryLimit))
    }

    def mkRepresentation(sizeOfInterest: Int, memoryLimit: Long): MultiStateRepresentation = {
      val evaluation = new SimplePerformanceEvaluation(
        sizeOfInterest = sizeOfInterest, evaluationTrials = evaluationTrials, nonsenseFitness = -1.0,
        resourceUsage = timeLimitedResourceUsage(timeLimitInMillis),
        sizeF = sizeF, breakingMemoryUsage = memoryLimit
      )
      MultiStateRepresentation(
        totalSizeTolerance = totalSizeTolerance,
        singleSizeTolerance = singleSizeTolerance,
        stateTypes = gpEnv.stateTypes, outputTypes = outputTypes, evaluation = evaluation)
    }

    FileInteraction.runWithAFileLogger(s"$recordDirPath/runLog.txt") { logger =>
      import logger._

      println(s"Starting task: ${problemConfig.problemName}")

      printSection("Configuration"){
        println(s"[sizePolicy] $evalSizePolicy")
        println(gpEnv.show)
        println(config.show)
      }

      printSection("Function map"){
        gpEnv.functionMap.foreach { case (t, comps) =>
          println(s"$t -> ${comps.mkString("{", ", ", "}")}")
        }
      }


      val refRepresentation = mkRepresentation(
        evalSizePolicy.referenceSize,
        calcMemoryLimit(evalSizePolicy.referenceSize))
      var (evalSize, MemoryUsage(memoryLimit)) = newEvalSize()
      var representation = mkRepresentation(evalSize, memoryLimit)

      def updateEvalSize(): Unit = {
        val newSize = newEvalSize()
        println(s"new eval size = $newSize")
        evalSize = newSize._1
        memoryLimit = newSize._2.amount
        representation = mkRepresentation(evalSize, memoryLimit)
      }

      val operators = IS(
        library.simpleCrossOp -> crossoverP,
        library.simpleMutateOp(newTreeMaxDepth = 3) -> mutateP,
        library.copyOp -> copyP,
        library.constantFolding -> constFoldP
      )

      def showPattern(ind: MultiStateInd): String ={
        val memoryLimit = representation.evaluation.breakingMemoryUsage

        representation.individualToPattern(ind).takeWhile{
          case (MemoryUsage(mem), _) => mem < memoryLimit
        }.take(runnerConfig.previewPatternLen).toList.map {
          case (_, v) => escapeStrings(displayValue(v))
        }.mkString(", ")
      }

      val generations = EvolutionaryOptimizer[MultiStateInd]().optimize(
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
                showPattern(ind)
              }
              FileInteraction.saveMultiIndToFile(s"$recordDirPath/timeoutIndividual.serialized")(ind)
              FileInteraction.writeToFile(s"$recordDirPath/bestIndividual.txt"){
                representation.showIndividualMultiLine(ind)
              }

              // We might also be interested in the value
              MultiStateRepresentation.saveExtrapolation(problemConfig ,ind, evalSize, memoryLimit,
                s"$recordDirPath/timeoutValue")
              renameResultDir("timeout")

              System.exit(0)
              throw new Exception("Timed out!")
          }
        },
        threadNum = threadNum,
        random = rand,
        evalProgressCallback = { i =>
          evalProgressCallback(s"GP Evaluation progress: $i")
        },
        bufferEvaluation = evalSizePolicy match {
          case _: FixedEvalSize => true
          case _: VariedEvalSize => false
        }
      ).map{ pop =>
        evalSizePolicy match {
          case _: VariedEvalSize =>
            val inds = pop.individuals.zipWithIndex.map { case (indData, p) =>
              val eval = refRepresentation.fitnessEvaluation(indData.ind)._1
              evalProgressCallback(s"Ref evaluation progress: $p")
              indData.copy(evaluation = eval)
            }
            ReEvaluatedPop(pop.individuals, inds)
          case _: FixedEvalSize =>
            ReEvaluatedPop(pop.individuals, pop.individuals)
        }
      }


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
        FileInteraction.saveMultiIndToFile(s"$recordDirPath/bestIndividual$timeString.serialized")(indData.ind)
        FileInteraction.writeToFile(s"$recordDirPath/bestIndividual.txt"){
          representation.showIndividualMultiLine(indData.ind)
        }

        MultiStateRepresentation.saveExtrapolation(problemConfig, indData.ind,
          evalSize, memoryLimit, s"$recordDirPath/bestInput$timeString")
      }

      generations.takeWhile(pop => {
        updateEvalSize()

        nonIncreasingTime += 1
        bestSoFar match {
          case Some(previousBest) =>
            if (pop.bestInd.evaluation.fitness > previousBest.evaluation.fitness) {
              nonIncreasingTime = 0
              setBestInd(pop.bestInd)
            }
          case None => setBestInd(pop.bestInd)
        }
        println(s"Last fitness increase: $nonIncreasingTime generations ago.")

        val shouldStop = {
          val timeInSec = (System.nanoTime() - startTime) / 1e9
          maxNonIncreaseGen.exists(nonIncreasingTime > _) ||
          maxFuzzingTimeSec.exists(timeInSec > _)
        }
        !shouldStop
      }).zipWithIndex.foreach { case (pop, i) =>

        val bestData = pop.bestInd
        val bestEval = {
          val bestInd = bestData.ind
          refRepresentation.fitnessEvaluation(bestInd)._1
        }

        monitorCallback(MonitoringData(pop.averageFitness, bestEval.fitness, bestEval.performance))
        saveMonitor(s"$recordDirPath/monitor.png")

        println("------------")
        print("[" + TimeTools.nanoToSecondString(System.nanoTime() - startTime) + "]")
        println(s"Generation ${i + 1}")
        println(s"Best Result: ${bestEval.showAsLinearExpr}, Created by ${bestData.history.birthOp}")
        representation.printIndividualMultiLine(println)(bestData.ind)
        println(s"Best Individual Pattern: ${showPattern(bestData.ind)}, ...")
        println(s"Average Size: ${representation.populationAverageSize(pop.refEvaluations.map(_.ind))}")
        println(s"Average Fitness: ${pop.averageFitness}")
        println(s"Fitness Variation: ${pop.fitnessStdDiv}")
        print("Distribution: ")
        println {
          representation.frequencyRatioStat(pop.refEvaluations.map(_.ind)).take(10).map {
            case (s, f) => s"$s -> ${"%.3f".format(f)}"
          }.mkString(", ")
        }
      }

      println("Evolution Finished!")
      try{
        val performance = bestSoFar.get.evaluation.performance
        renameResultDir(s"$performance")
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
