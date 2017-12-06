package benchmarks

import edu.utexas.stac.Cost
import patsyn.Runner.RunnerConfig
import patsyn._
import patsyn.StandardSystem._
import Supernova._

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


  def collect_hashing_smear: ProblemConfig = {
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

  def immutableSet_copyOf: ProblemConfig = {
    ProblemConfig(
      "ImmutableSet.copyOf",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(v) => v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val vecInt = vec.map(_.asInstanceOf[IntValue].value)
          val list = TestGuava.seqToArrayList(vecInt)
          measureCost{
            handleException(()){
              TestGuava.arrayListToSet(list)
            }
          }
      }
    )
  }

  def immutableBiMap_copyOf: ProblemConfig = {
    ProblemConfig(
      "ImmutableBiMap.copyOf",
      outputTypes = IS(EVect(EPair(EInt, EInt))),
      sizeF = {
        case IS(v) => v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(pairs)) =>
          val list = TestGuava.toArrayListPair(pairs.map {
            case PairValue((IntValue(k), IntValue(v))) => (new Integer(k), new Integer(v))
          })
          measureCost {
            handleException(()) {
              TestGuava.arrayListToBiMap(list)
            }
          }
      }
    )
  }

  def immutableBiMap_inverse: ProblemConfig = {
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
            val map = TestGuava.arrayListToBiMap(TestGuava.toArrayListPair(disdinctKey))
            measureCost(map.inverse())
          }
      }
    )
  }


  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    fuzzProblem(
      immutableSet_copyOf,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = VariedEvalSize.choppedGaussian(rand, 500)), rand)
  }

  def main(args: Array[String]): Unit = {
    SimpleMath.processMap(args,
      0 to 60, processNum = 14,
      mainClass = this){
      i => runExample(i, useGUI = false)
    }
//    runExample(15, useGUI = true)
  }
}
