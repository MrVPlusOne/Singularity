package patsyn


import StandardSystem._
import measure.{TimeMeasureExamples, TimeMeasurement, TimeTools}
import patsyn.GeneticOperator.ExprGen

import scala.util.Random

object TestRun {

  def main(args: Array[String]): Unit = {
    import javax.swing._
    import gui._

    val frame = new JFrame("GP Monitor") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setVisible(true)
    }

    var dataCollected = IS[MonitoringData]()
    runExample(FuzzingExample.phpHashCollision,
      monitorCallback = d => {
        dataCollected :+= d

        val avFitLine = dataCollected.map(_.averageFitness)
        val bestFitness = dataCollected.map(_.bestFitness)
        val bestPerformance = dataCollected.map(_.bestPerformance)

        val chart = ListPlot.plot(
          "best performance" -> makeXY(bestPerformance),
          "best fitness" -> makeXY(bestFitness),
          "average fitness" -> makeXY(avFitLine))("Performance Curve", "Generations", "Evaluation")
        frame.setContentPane(new MonitorPanel(chart, 10, (600, 450)))
        frame.pack()
      })
  }

  def makeConstMap(pairs: (EType, IS[Random => EValue])*): Map[EType, IS[ExprGen[EConst]]] = {
    pairs.map{ case (t, fs) =>
      t -> fs.map(f =>ExprGen(t, r => EConst(t, f(r))))
    }.toMap
  }

  case class MonitoringData(averageFitness: Double, bestFitness: Double, bestPerformance: Double)

  def makeXY(ys: IS[Double]): IS[(Double, Double)] = {
    ys.indices.map{ i => (i+1).toDouble -> ys(i)}
  }

  def runExample(example: FuzzingExample, monitorCallback: MonitoringData => Unit): Unit = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector()),
      EVect(EVect(EInt)) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    val gpEnv = GPEnvironment(constMap, functions, stateTypes)
    val library = MultiStateGOpLibrary(gpEnv, example.outputTypes)

    println("[Function map]")
    gpEnv.functionMap.foreach { case (t, comps) =>
      println(s"$t -> ${comps.mkString("{", ", ", "}")}")
    }
    println("[End of Function map]")

    val recordDirPath = {
      import java.util.Calendar
      val dateTime = Calendar.getInstance().getTime
      s"results/$dateTime"
    }

    for (seed <- 2 to 5) {
      val evaluation = new SimplePerformanceEvaluation(
        sizeOfInterest = 600, maxTrials = 3, nonsenseFitness = -1.0,
        resourceUsage = example.resourceUsage, sizeF = example.sizeF, maxMemoryUsage = 600*10
      )
      val representation = MultiStateRepresentation(totalSizeTolerance = 60, singleSizeTolerance = 30,
        stateTypes = stateTypes, outputTypes = example.outputTypes, evaluation = evaluation)
      val optimizer = EvolutionaryOptimizer(representation)
      val operators = IS(
        library.simpleCrossOp -> 0.4,
        library.simpleMutateOp(newTreeMaxDepth = 3) -> 0.5,
        library.copyOp -> 0.1
      )

      val generations = optimizer.optimize(
        populationSize = 500, tournamentSize = 7, neighbourSize = 245,
        initOperator = library.initOp(maxDepth = 3),
        operators = operators,
        evaluation = ind => {
          representation.fitnessEvaluation(ind)._1
        },
        threadNum = 8,
        randSeed = seed
      )

      FileLogger.runWithAFileLogger(s"$recordDirPath/testResult[seed=$seed].txt") { logger =>
        import logger._

        val parameterInfo = ""
        println(parameterInfo)

        val startTime = System.nanoTime()
        generations.take(250).zipWithIndex.foreach { case (pop, i) =>
          val best = pop.bestSoFar
          val data = MonitoringData(pop.averageFitness, best.evaluation.fitness, best.evaluation.performance)
          monitorCallback(data)

          println("------------")
          print("[" + TimeTools.nanoToSecondString(System.nanoTime() - startTime) + "]")
          println(s"Generation ${i+1}")
          println(s"Best Result: ${best.evaluation.showAsLinearExpr}, Created by ${best.history.birthOp.name}")
          representation.printIndividualMultiLine(println)(best.ind)
          val firstSevenInputs = representation.fitnessEvaluation(best.ind)._2.take(7).map(
            _.mkString("< ", " | ", " >")).mkString(", ")
          println(s"Best Individual Pattern: $firstSevenInputs, ...")
          println(s"Diversity: ${pop.fitnessMap.keySet.size}")
          println(s"Average Size: ${representation.populationAverageSize(pop)}")
          println(s"Average Fitness: ${pop.averageFitness}")
          println(s"Fitness Variation: ${pop.fitnessStdDiv}")
          print("Distribution: ")
          println(representation.frequencyRatioStat(pop.individuals.map(_.ind)).take(10).map {
            case (s, f) => s"$s -> ${"%.3f".format(f)}"
          }.mkString(", "))
        }
      }
    }
  }

}
