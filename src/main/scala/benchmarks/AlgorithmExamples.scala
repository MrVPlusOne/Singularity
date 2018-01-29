package benchmarks

import java.util.Random

import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._

object AlgorithmExamples {
  def giftWrapping = {
    ProblemConfig(
      "ds.convexhull.giftwrapping",
      outputTypes = IS(EVect(EPair(EInt, EInt))),
      sizeF = {
        case IS(vec) =>
          vec.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          import patbench.ds.edu.utexas.stac.DataStructureHarness
          import patbench.ds.edu.utexas.stac.geometry.Point

          val points = vec.map{
            case PairValue((IntValue(x), IntValue(y))) =>
              // Make sure cross product computation never overflows
              new Point(x % 65536, y % 65536)
          }.toSet.toArray
          try {
            BenchmarkSet.measureCost {
              DataStructureHarness.giftWrappingHarness(points)
            }
          }
          catch {
            case _: Exception => 0
          }
      }
    )
  }

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.standardSupernova.fuzzProblem(
      giftWrapping,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(10), timeLimitInMillis = 10000), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(0, true)
  }
}
