package benchmarks

import java.util
import java.util.Map

import edu.utexas.stac.Cost
import org.apache.commons.lang3.tuple.ImmutablePair
import patbench.guava.common.collect._
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

  def immutableBiMap_inverse = {
    ProblemConfig(
      outputTypes = IS(EVect(EPair(EInt, EInt))),
      sizeF = {
        case IS(v) => v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(pairs)) =>
//          val list = new util.ArrayList[util.Map.Entry[Int,Int]]()
//          pairs.foreach{
//            case IS(PairValue((IntValue(k), IntValue(v)))) =>
//              list.add(new ImmutablePair(k,v))
//          }

          measureCost {
//            val a = ImmutableBiMap.copyOf(list: java.lang.Iterable[util.Map.Entry[Int, Int]])
            val a = TestImmutableBiMap.makeMap(pairs.map{
              case PairValue((IntValue(k), IntValue(v))) => (k,v)
            })
            a.inverse()
          }
      }
    )
  }

  def main(args: Array[String]): Unit = {
    val seed = 3
    val rand = new Random(seed)
    sledgehammerProblem(
      immutableBiMap_inverse,
      RunnerConfig().copy(randomSeed = seed, ioId = seed), ExecutionConfig(), rand)
  }
}
