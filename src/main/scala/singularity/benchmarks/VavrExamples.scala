package singularity.benchmarks

import java.util.Random

import singularity.benchmarks.AllTogether.ConfigGen
import singularity.Runner.RunnerConfig
import singularity.StandardSystem._
import singularity._
import visual.PatternPlot.showResourceUsageChart

object VavrExamples {

  def handleException[A](default: A)(f: => A): A = {
    try{
      f
    }catch {
      case _: Throwable => default //fixme: Change this to more specific exceptions
    }
  }

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
          handleException(0l) {
            BenchmarkSet.measureCost {
              pq.grouped(Math.floorMod(s, pq.size()))
            }
          }
      }
    )
  }

  def treeSetIntersect: ProblemConfig = {
    ProblemConfig(
      "vavr.treeset.intersect",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          val set0 = TestVavr.arrayToTreeSet(vec0.map { case IntValue(i) => i }.toArray)
          val set1 = TestVavr.arrayToTreeSet(vec1.map { case IntValue(i) => i }.toArray)

          handleException(0l) {
            BenchmarkSet.measureCost {
              set0.intersect(set1)
            }
          }
      }
    )
  }

  def treeSetUnion: ProblemConfig = {
    ProblemConfig(
      "vavr.treeset.union",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          val set0 = TestVavr.arrayToTreeSet(vec0.map { case IntValue(i) => i }.toArray)
          val set1 = TestVavr.arrayToTreeSet(vec1.map { case IntValue(i) => i }.toArray)

          handleException(0l) {
            BenchmarkSet.measureCost {
              set0.union(set1)
            }
          }
      }
    )
  }

  def hashSetIntersect: ProblemConfig = {
    ProblemConfig(
      "vavr.hashset.intersect",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          val set0 = TestVavr.arrayToHashSet(vec0.map { case IntValue(i) => i }.toArray)
          val set1 = TestVavr.arrayToHashSet(vec1.map { case IntValue(i) => i }.toArray)

          handleException(0l) {
            BenchmarkSet.measureCost {
              set0.intersect(set1)
            }
          }
      }
    )
  }

  def hashSetUnion: ProblemConfig = {
    ProblemConfig(
      "vavr.hashset.union",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          val set0 = TestVavr.arrayToHashSet(vec0.map { case IntValue(i) => i }.toArray)
          val set1 = TestVavr.arrayToHashSet(vec1.map { case IntValue(i) => i }.toArray)

          handleException(0l) {
            BenchmarkSet.measureCost {
              set0.union(set1)
            }
          }
      }
    )
  }

  def linkedHashSetIntersect: ProblemConfig = {
    ProblemConfig(
      "vavr.linkedhashset.intersect",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          val set0 = TestVavr.arrayToLinkedHashSet(vec0.map { case IntValue(i) => i }.toArray)
          val set1 = TestVavr.arrayToLinkedHashSet(vec1.map { case IntValue(i) => i }.toArray)

          handleException(0l) {
            BenchmarkSet.measureCost {
              set0.intersect(set1)
            }
          }
      }
    )
  }

  def linkedHashSetUnion: ProblemConfig = {
    ProblemConfig(
      "vavr.linkedhashset.union",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          val set0 = TestVavr.arrayToLinkedHashSet(vec0.map { case IntValue(i) => i }.toArray)
          val set1 = TestVavr.arrayToLinkedHashSet(vec1.map { case IntValue(i) => i }.toArray)

          handleException(0l) {
            BenchmarkSet.measureCost {
              set0.union(set1)
            }
          }
      }
    )
  }

  def all: IS[ProblemConfig] = IS(treeSetIntersect, treeSetUnion, hashSetIntersect, hashSetUnion, linkedHashSetIntersect, linkedHashSetUnion)

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
   Supernova.standardSupernova.fuzzProblem(
      treeSetIntersect,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(200)), rand)
  }

//  def main(args: Array[String]): Unit = {
//    runExample(0, true)
//    val ind = FileInteraction.readMultiIndFromFile("/home/grieve/scratch/results/vavr.linkedhashset.union[performance=1048047.0][ioId=125,seed=125](17-12-15-11:26:57)/bestIndividual.serialized", StandardSystem.funcMap)
//    showResourceUsageChart(linkedHashSetUnion, ind, 10000, 50)
//  }
}
