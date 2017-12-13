package benchmarks

import java.util.Random

import edu.utexas.stac.Cost
import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._
import visual.PatternPlot.showResourceUsageChart

object VavrExamples {

  def pqGroup: ProblemConfig = {
    ProblemConfig(
      "vavr.pq.group",
      outputTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(v, _) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(s)) =>
          val arr = vec.map { case IntValue(i) => i }.toArray
          val pq = TestVavr.arrayToPriorityQueue(arr)
          try {
            Cost.reset()
            pq.grouped(Math.floorMod(s, pq.size()))
            Cost.read()
          } catch {
            case _: Throwable => 0
          }
      }
    )
  }

  def treeSetAdd: ProblemConfig = {
    ProblemConfig(
      "vavr.treeset.add",
      outputTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(v, _) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(s)) =>
          val nums = vec.map { case IntValue(i) => i }.toArray
          val set = TestVavr.arrayToTreeSet(nums)

          Cost.reset()
          set.add(s)
          Cost.read()
      }
    )
  }

  def hashSetAdd: ProblemConfig = {
    ProblemConfig(
      "vavr.hashset.add",
      outputTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(v, _) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(s)) =>
          val nums = vec.map { case IntValue(i) => i }.toArray
          val set = TestVavr.arrayToHashSet(nums)

          Cost.reset()
          set.add(s)
          Cost.read()
      }
    )
  }

  def linkedHashSetAdd: ProblemConfig = {
    ProblemConfig(
      "vavr.linkedhashset.add",
      outputTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(v, _) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(s)) =>
          val nums = vec.map { case IntValue(i) => i }.toArray
          val set = TestVavr.arrayToLinkedHashSet(nums)

          Cost.reset()
          set.add(s)
          Cost.read()
      }
    )
  }

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.fuzzProblem(
      linkedHashSetAdd,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(500)), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(0, true)
//    val ind = FileInteraction.readMultiIndFromFile("results/vavr.hashset.add[performance=111.0][ioId=2,seed=2](17-12-13-17:28:10)/bestIndividual.serialized", StandardSystem.funcMap)
//    showResourceUsageChart(hashSetAdd, ind, 2000, 50)
  }
}
