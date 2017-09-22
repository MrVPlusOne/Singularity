package patsyn

import StandardSystem._
import patsyn.Evolution.IndividualEvaluation
import patsyn.GeneticOpLibrary.ExprGen

import scala.util.Random

object TestRun {
  val failedIterFitness = 0.0

  case class FuzzingExample(seedTypes: IS[EType], sizeF: IS[EValue] => Int, resourceUsage: IS[EValue] => Double)

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
        case _ => notPossible()
      },
      resourceUsage = {
        case IS(VectValue(v)) =>
          val c = new Counter()
          Examples.insertionSort(c)(toIntVect(v))
          c.read()
        case _ => notPossible()
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
        case _ => notPossible()
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val c = new Counter()
          Examples.quickSort(c, choosePivot)(toIntVect(vec))
          c.read()
        case _ => notPossible()
      }
    )
  }

  def randomQuickSortExample(seed: Int): FuzzingExample = {
    FuzzingExample(
      seedTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) =>
          v.length
        case _ => notPossible()
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val random = new Random(seed)
          val c = new Counter()
          Examples.quickSort(c, xs => xs(random.nextInt(xs.length)))(toIntVect(vec))
          c.read()
        case _ => notPossible()
      }
    )
  }

  def listSearchExample: FuzzingExample = {
    FuzzingExample(
      seedTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(VectValue(v), IntValue(_)) => v.length
        case _ => notPossible()
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(idx)) =>
          val c = new Counter()
          Examples.listSearchAndCopy(c)(toIntVect(vec), idx)
          c.read()
        case _ => notPossible()
      }
    )
  }

  def makeConstMap(pairs: (EType, Random => EValue)*): Map[EType, IS[ExprGen[EConst]]] = {
    pairs.map{ case (t, f) =>
      t -> IS(ExprGen(t, r => EConst(t, f(r))))
    }.toMap
  }

  def main(args: Array[String]): Unit = {
    val example = quickSortExample

    val constMap = makeConstMap(
      EInt -> (r => r.nextInt(7)),
      EVect(EInt) -> (_ => Vector()),
      EVect(EVect(EInt)) -> (_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val library = new GeneticOpLibrary(constMap, functions, example.seedTypes)

    println("[Function map]")
    library.functionMap.foreach{ case (t, comps) =>
      println(s"$t -> ${comps.mkString("{",", ","}")}")
    }
    println("[End of Function map]")

    MamLink.runWithALinkOnMac{ link =>

      val eval: (IS[Expr], IS[Expr]) => ((Double, Double), Stream[IS[EValue]]) = (seeds, iters) => {
        val seedSizeFringe = 12
        val programSizeFringe = 20
        val penaltyFactor = seeds.map(s => Evaluation.gaussianSquared(seedSizeFringe)(s.astSize)).product *
          iters.map(iter => Evaluation.gaussianSquared(programSizeFringe)(iter.astSize)).product

        val (v, stream) = new SimpleEvaluation(sizeOfInterest = 400, maxTrials = 3).evaluateAPattern(
          example.resourceUsage, example.sizeF
        )(seeds, iters)

        ((v * penaltyFactor,v), stream)
      }


      val evolution = new Evolution()
      val generations = evolution.evolveAFunction(
        populationSize = 1000, tournamentSize = 8,
        randSeed = 1,
        initOperator = library.initOp(maxDepth = 3),
        operators = IS(
          library.simpleCrossOp(0.2) -> 0.6,
          library.simpleMutateOp(newTreeMaxDepth = 3, 0.2) -> 0.3,
          library.copyOp -> 0.1
        ),
        evaluation = ind => {
          val (fitness, performance) = eval(ind.seed, ind.iter)._1
          IndividualEvaluation(ind, fitness, performance)
        }
      )

      FileLogger.runWithAFileLogger("testResult.txt") { logger =>
        import logger._

        generations.take(30).zipWithIndex.foreach { case (pop, i) =>
          println("------------")
          println(s"Generation ${i + 1}")
          val best = pop.bestSoFar
          println(s"Best Individual: ${best.showAsLinearExpr}")
          link.logInteraction = true
          val firstFiveInputs = eval(best.ind.seed, best.ind.iter)._2.take(5).map(
            _.mkString("< ", " | ", " >")).mkString(", ")
          link.logInteraction = false
          println(s"Best Individual Pattern: $firstFiveInputs, ...")
          println(s"Average Size: ${pop.averageSize}")
          println(s"Average Fitness: ${pop.averageFitness}")
          print("Distribution: "); println(pop.frequencyRatioStat.take(12).map{
            case(s, f) => s"$s -> ${"%.3f".format(f)}"
          }.mkString(", "))
        }
      }
    }
  }
}
