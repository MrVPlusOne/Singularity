package benchmarks

import java.util.Random

import edu.utexas.stac.Cost
import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._
import visual.PatternPlot.showResourceUsageChart

object VavrExamples {

  def vectIntToPriorityQueue(vec: IS[EValue]) = {
    vec.map { case IntValue(i) => new Integer(i) }.foldLeft(patbench.vavr.collection.PriorityQueue.empty[Integer]())((q, i) => q.enqueue(i))
  }

  def group: ProblemConfig = {
    ProblemConfig(
      "vavr.group",
      outputTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(v, _) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(s)) =>
          val pq = vectIntToPriorityQueue(vec)
          try {
            Cost.reset()
            pq.grouped(math.abs(s))
            Cost.read()
          } catch {
            case _: Throwable => 0
          }
      }
    )
  }

  def merge: ProblemConfig = {
    ProblemConfig(
      "vavr.merge",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          val pq0 = vectIntToPriorityQueue(vec0)
          val pq1 = vectIntToPriorityQueue(vec1)
          try {
            Cost.reset()
            pq0.merge(pq1)
            Cost.read()
          } catch {
            case _: Throwable => 0
          }
      }
    )
  }

  def toList: ProblemConfig = {
    ProblemConfig(
      "vavr.tolist",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(v) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val pq = vectIntToPriorityQueue(vec)
          try {
            Cost.reset()
            pq.toList()
            Cost.read()
          } catch {
            case _: Throwable => 0
          }
      }
    )
  }

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.fuzzProblem(
      toList,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(200)), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(0, true)
//    val ind = FileInteraction.readMultiIndFromFile("results/vavr.merge[performance=218.0][ioId=0,seed=0](17-12-12-15:44:57)/bestIndividual.serialized", StandardSystem.funcMap)
//    showResourceUsageChart(merge, ind, 2000)
  }
}
