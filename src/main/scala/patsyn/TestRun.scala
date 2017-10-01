package patsyn


import StandardSystem._
import measure.{TimeMeasureExamples, TimeMeasurement, TimeTools}
import patsyn.EvolutionRepresentation.IndividualEvaluation
import patsyn.GeneticOperator.ExprGen

import scala.util.Random

object TestRun {

  case class FuzzingExample(seedTypes: IS[EType],
                            sizeF: PartialFunction[IS[EValue], Int],
                            resourceUsage: PartialFunction[IS[EValue], Double])

  def notPossible[T](): T = throw new Exception("Not possible!")

  def toIntVect(v: Vector[EValue]): Vector[Int] = {
      v.asInstanceOf[Vector[IntValue]].map(_.value)
  }

  def insertionSortExample: FuzzingExample = {
    FuzzingExample(
      seedTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) =>
          v.length
      },
      resourceUsage = {
        case IS(VectValue(v)) =>
          val c = new Counter()
          Examples.insertionSort(c)(toIntVect(v))
          c.read()
      }
    )
  }

  def quickSortExample: FuzzingExample = {
    def choosePivot(xs: IS[Int]): Int = {
      xs(xs.length/2) // choose middle
//      xs(xs.length/3) // 1/3
    }

    FuzzingExample(
      seedTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) =>
          v.length
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val c = new Counter()
          Examples.quickSort(c, choosePivot)(toIntVect(vec))
          c.read()
      }
    )
  }

  def randomQuickSortExample(seed: Int): FuzzingExample = {
    FuzzingExample(
      seedTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) =>
          v.length
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val random = new Random(seed)
          val c = new Counter()
          Examples.quickSort(c, xs => xs(random.nextInt(xs.length)))(toIntVect(vec))
          c.read()
      }
    )
  }

  def listSearchExample: FuzzingExample = {
    FuzzingExample(
      seedTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(VectValue(v), IntValue(_)) => v.length
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(idx)) =>
          val c = new Counter()
          Examples.listSearchAndCopy(c)(toIntVect(vec), idx)
          c.read()
      }
    )
  }

  def vectIntToString(vec: VectValue): String = {
    String.valueOf(vectIntToCharArray(vec))
  }

  def vectIntToCharArray(vec: VectValue): List[Char] = {
    vec.value.map { i =>
      (i.asInstanceOf[IntValue].value % 256).toChar
    }.toList
  }

  def phpHashTableExample(timeout: TimeMeasurement.DoubleAsMillis): FuzzingExample = {
    val example = TimeMeasureExamples.phpHashExampleNoFile
    FuzzingExample(
      seedTypes = IS(EVect(EVect(EInt))),
      sizeF = {
        case IS(VectValue(strings)) =>
          strings.map(s => s.asInstanceOf[VectValue].value.length).sum
        case _ => notPossible()
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val strings = vec.map(v => vectIntToString(v.asInstanceOf[VectValue]))
          example.measure(strings, timeout)
      }
    )
  }

  def phpHashCollision: FuzzingExample = {
    FuzzingExample(
      seedTypes = IS(EVect(EVect(EInt))),
      sizeF = {
        case IS(VectValue(strings)) =>
          strings.map(s => s.asInstanceOf[VectValue].value.length).sum
        case _ => notPossible()
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val hashes = vec.map(v => {
            vectIntToCharArray(v.asInstanceOf[VectValue])
          }).toSet.toIndexedSeq.map(hashFunc)

          hashes.groupBy(identity).values.map{
            elems => elems.length - 1
          }.sum
      }
    )
  }

  def hashFunc(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = 5381
    for(i <- cs.indices){
      hash = ((hash << 5) + hash) + cs(i)
    }
    hash
  }

  def makeConstMap(pairs: (EType, IS[Random => EValue])*): Map[EType, IS[ExprGen[EConst]]] = {
    pairs.map{ case (t, fs) =>
      t -> fs.map(f =>ExprGen(t, r => EConst(t, f(r))))
    }.toMap
  }

  case class MonitoringData(averageFitness: Double, bestFitness: Double, bestPerformance: Double)

  def runExample(monitorCallback: MonitoringData => Unit): Unit = {
    val example = phpHashCollision

    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector()),
      EVect(EVect(EInt)) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val gpEnv = GPEnvironment(constMap, functions, example.seedTypes)
    val library = SingleStateGOpLibrary(gpEnv)

    println("[Function map]")
    gpEnv.functionMap.foreach { case (t, comps) =>
      println(s"$t -> ${comps.mkString("{", ", ", "}")}")
    }
    println("[End of Function map]")


    val eval: (IS[Expr], IS[Expr]) => ((Double, Double), Stream[IS[EValue]]) = (seeds, iters) => {
      val seedSizeFringe = 15
      val programSizeFringe = 30
      val penaltyFactor = seeds.map(s => Evaluation.gaussianSquared(seedSizeFringe)(s.astSize)).product *
        iters.map(iter => Evaluation.gaussianSquared(programSizeFringe)(iter.astSize)).product

      val (v, stream) = new SimpleEvaluation(sizeOfInterest = 600, maxTrials = 3, nonsenseFitness = -1.0).evaluateAPattern(
        example.resourceUsage, example.sizeF
      )(seeds, iters)

      ((v * penaltyFactor, v), stream)
    }


    import java.util.Calendar
    import java.io._
    val dateTime = Calendar.getInstance().getTime
    new File("results/" + dateTime.toString).mkdir()

    for (seed <- 2 to 5) {
      val representation = SingleStateRepresentation
      val optimizer = EvolutionaryOptimizer(representation)
      val generations = optimizer.optimize(
        populationSize = 10000, tournamentSize = 7, neighbourSize = 4000,
        initOperator = library.initOp(maxDepth = 3),
        operators = IS(
          library.simpleCrossOp(0.2) -> 0.5,
          library.simpleMutateOp(newTreeMaxDepth = 3, 0.2) -> 0.4,
          library.copyOp -> 0.1
        ),
        evaluation = ind => {
          val (fitness, performance) = eval(ind.seed, ind.iter)._1
          IndividualEvaluation(fitness, performance)
        },
        threadNum = 8,
        randSeed = seed
      )

      FileLogger.runWithAFileLogger(s"results/$dateTime/testResult[$seed].txt") { logger =>
        import logger._

        val parameterInfo = ""
        println(parameterInfo)

        val startTime = System.nanoTime()
        generations.take(100).zipWithIndex.foreach { case (pop, i) =>
          val best = pop.bestSoFar
          val data = MonitoringData(pop.averageFitness, best.evaluation.fitness, best.evaluation.performance)
          monitorCallback(data)

          println("------------")
          print("[" + TimeTools.nanoToSecondString(System.nanoTime() - startTime) + "]")
          println(s"Generation ${i+1}")
          println(s"Best Individual: ${representation.showIndData(best)}")
          val firstFiveInputs = eval(best.ind.seed, best.ind.iter)._2.take(5).map(
            _.mkString("< ", " | ", " >")).mkString(", ")
          println(s"Best Individual Pattern: $firstFiveInputs, ...")
          println(s"Best Individual created by: ${best.history.birthOp.name}, HistoryLen: ${best.history.historyLength}")
          println(s"Diversity: ${pop.fitnessMap.keySet.size}")
          println(s"Average Size: ${representation.populationAverageSize(pop)}")
          println(s"Average Fitness: ${pop.averageFitness}")
          println(s"Fitness Variation: ${pop.fitnessStdDiv}")
          print("Distribution: ")
          println(representation.frequencyRatioStat(pop.individuals.map(_.ind)).take(12).map {
            case (s, f) => s"$s -> ${"%.3f".format(f)}"
          }.mkString(", "))
        }
      }
    }
  }

  def makeXY(ys: IS[Double]): IS[(Double, Double)] = {
    ys.indices.map{ i => (i+1).toDouble -> ys(i)}
  }

  def main(args: Array[String]): Unit = {
    import javax.swing._
    import gui._

    val frame = new JFrame("GP Monitor") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setVisible(true)
    }


    var dataCollected = IS[MonitoringData]()
    runExample(monitorCallback = d => {
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
}
