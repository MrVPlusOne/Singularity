package benchmarks

import edu.utexas.stac.Cost
import patsyn.Runner.RunnerConfig
import patsyn._
import patsyn.StandardSystem._
import Sledgehammer._

import scala.util.Random

object GuavaExamples {
  def measureCost[A](f: => A): Long = {
    Cost.reset()
    f
    Cost.read()
  }

  def handleException[A](default: A)(f: => A): A = {
    try{
      f
    }catch {
      case _: IllegalArgumentException => default
    }
  }

  def immutableBiMap_copyOf = {
    ProblemConfig(
      "ImmutableBiMap.copyOf",
      outputTypes = IS(EVect(EPair(EInt, EInt))),
      sizeF = {
        case IS(v) => v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(pairs)) =>
          val list = TestImmutableBiMap.toArrayListPair(pairs.map {
            case PairValue((IntValue(k), IntValue(v))) => (new Integer(k), new Integer(v))
          })
          measureCost {
            handleException(()) {
              TestImmutableBiMap.arrayListToBiMap(list)
            }
          }
      }
    )
  }

  def immutableBiMap_inverse = {
    ProblemConfig(
      "ImmutableBiMap.inverse",
      outputTypes = IS(EVect(EPair(EInt, EInt))),
      sizeF = {
        case IS(v) => v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(pairs)) =>
          val disdinctKey = pairs.map {
            case PairValue((IntValue(k), IntValue(v))) => (new Integer(k), new Integer(v))
          }.toMap.toVector

          handleException(0.0) {
            val map = TestImmutableBiMap.arrayListToBiMap(TestImmutableBiMap.toArrayListPair(disdinctKey))
            measureCost(map.inverse())
          }
      }
    )
  }


  def runExample(seed: Int): Unit = {
    val rand = new Random(seed)
    sledgehammerProblem(
      immutableBiMap_inverse,
      RunnerConfig().copy(randomSeed = seed, ioId = seed), ExecutionConfig(sizeOfInterest = 1000), rand)
  }

  def main(args: Array[String]): Unit = {
    SimpleMath.processMap(args,
      10 to 20, processNum = 2,
      mainClass = this){
      i => runExample(i)
    }
  }
}
