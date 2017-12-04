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


  def collect_hashing_smear = {
    val C1 = 0xcc9e2d51
    val C2 = 0x1b873593

    /*
     * This method was rewritten in Java from an intermediate step of the Murmur hash function in
     * http://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp, which contained the
     * following header:
     *
     * MurmurHash3 was written by Austin Appleby, and is placed in the public domain. The author
     * hereby disclaims copyright to this source code.
     */
    def smear(hashCode: Int): Int = {
      C2 * Integer.rotateLeft(hashCode * C1, 15)
    }

    ProblemConfig(
      "collect.Hashing.smear",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(v) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val hashes = vec.map{ case IntValue(i) => i}.distinct.map(smear)
          hashes.groupBy(identity).values.map{g =>
            val c = g.length - 1
            c*c
          }.sum
      }
    )
  }

  def collect_set = {
    ???
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
      collect_hashing_smear,
      RunnerConfig().copy(randomSeed = seed, ioId = seed),
      ExecutionConfig(sizeOfInterest = 1600, timeLimitInMillis = 20000), rand)
  }

  def main(args: Array[String]): Unit = {
    SimpleMath.processMap(args,
      0 to 100, processNum = 20,
      mainClass = this){
      i => runExample(i)
    }
  }
}
