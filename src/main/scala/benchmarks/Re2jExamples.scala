package benchmarks

import java.util.Random

import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._

object Re2jExamples {
  def reFind: ProblemConfig = {
    ProblemConfig(
      "re2j.find",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          import patbench.re2j.{Pattern, PatternSyntaxException}
          try {
            val pattern = Pattern.compile(FuzzingTaskProvider.vectIntToString(vec0))
            val str = FuzzingTaskProvider.vectIntToString(vec1)
            BenchmarkSet.measureCost {
              pattern.matcher(str).find()
            }
          } catch {
            case _: PatternSyntaxException => 0
          }
      }
    )
  }

  def reMatch: ProblemConfig = {
    ProblemConfig(
      "re2j.match",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          import patbench.re2j.{Pattern, PatternSyntaxException}
          try {
            val pattern = Pattern.compile(FuzzingTaskProvider.vectIntToString(vec0))
            val str = FuzzingTaskProvider.vectIntToString(vec1)
            BenchmarkSet.measureCost {
              pattern.matches(str)
            }
          } catch {
            case _: PatternSyntaxException => 0
          }
      }
    )
  }

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.fuzzProblem(
      reMatch,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(200)), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(0, true)

//        val ind = FileInteraction.readMultiIndFromFile("results/re2j.match[performance=76530.0][ioId=0,seed=0](17-12-17-14:51:25)/bestIndividual.serialized", StandardSystem.funcMap)
//    visual.PatternPlot.showResourceUsageChart(reMatch, ind, 10000, 50)
  }
}
