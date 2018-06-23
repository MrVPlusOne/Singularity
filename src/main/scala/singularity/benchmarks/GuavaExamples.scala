package singularity.benchmarks


import java.util.Comparator

import singularity.Runner.RunnerConfig
import singularity._
import singularity.StandardSystem._
import Supernova._
import BenchmarkSet._
import patbench.guava.common.collect.TreeMultiset

import scala.util.Random

object GuavaExamples {

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
      "guava.collect.Hashing.smear",
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

  def arrayListInputExample(name: String, f: java.util.ArrayList[Int] => Any): ProblemConfig = {
    ProblemConfig(
      name,
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(v: VectValue) => v.value.length
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val vecInt = vec.map(_.asInstanceOf[IntValue].value)
          val list = TestGuava.seqToArrayList(vecInt)
          measureCost{
            handleException(()){
              f(list)
            }
          }
      }
    )
  }

  def treeMultiSet_insertSequence: ProblemConfig = {
    ProblemConfig(
      "guava.TreeMultiset.add",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(v: VectValue) => v.value.length
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val vecInt = vec.map(_.asInstanceOf[IntValue].value)

          measureCost{
            handleException(()){
              val ms = TreeMultiset.create[Int](new Comparator[Int] {
                def compare(o1: Int, o2: Int): Int = o1-o2
              })
              vecInt.foreach(i => ms.add(i))
            }
          }
      }
    )
  }

  def immutableBiMap_copyOf: ProblemConfig = {
    ProblemConfig(
      "guava.ImmutableBiMap.copyOf",
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
      "guava.ImmutableBiMap.inverse",
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

  def immutableSet_copyOf = arrayListInputExample("guava.ImmutableSet.copyOf",
    TestGuava.arrayListToSet)

  def immutableMultiset_copyOf = arrayListInputExample("guava.ImmutableMultiset.copyOf",
    TestGuava.arrayListToMultiset)

  def all: IS[ProblemConfig] = IS(
    treeMultiSet_insertSequence, immutableBiMap_copyOf, immutableSet_copyOf, immutableMultiset_copyOf
  )

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    standardSupernova.fuzzProblem(
      treeMultiSet_insertSequence,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI, keepBestIndividuals = true),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(300), maxFuzzingTimeSec = Some(3600)), rand)
  }

  def testAverage(): Unit ={
    import StandardSystem._

    val prob = immutableBiMap_copyOf
    val rand = new Random(1)
    val pairs = VectValue(Vector.fill(600)((rand.nextInt(), rand.nextInt())))
    prob.resourceUsage(IS(pairs))
  }

  def fitCurveImmutableSet(): Unit ={
    val input = GuavaImmutableSet.generateInput()
    val iter = input.iterator()
    var size = 0
    var acc = Vector[IntValue]()
    val prob = immutableSet_copyOf
    while(iter.hasNext){
      size += 1
      acc = acc :+ IntValue(iter.next().toInt)
      val y = prob.resourceUsage(IS(acc))
      print((size, y) + ",")
    }
  }

  def main(args: Array[String]): Unit = {
    SimpleMath.processMap(args,
      0 to 30, processNum = 8,
      mainClass = this){
      i => runExample(i, useGUI = false)
    }
//    runExample(15, useGUI = true)

//    testAverage()

//    runExample(7, useGUI = true)
//
//    fitCurveImmutableSet()
  }
}
