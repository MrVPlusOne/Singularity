package benchmarks

import java.util.Random

import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._

object LuceneExamples {
  def regexMatch: ProblemConfig = {
    ProblemConfig(
      "lucene.regex",
      outputTypes = IS(EVect(EInt), EVect(EInt)),
      sizeF = {
        case IS(v0, v1) =>
          v0.memoryUsage.toInt + v1.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec0), VectValue(vec1)) =>
          import patbench.lucene.util.automaton.{RegExp, ByteRunAutomaton, TooComplexToDeterminizeException}
          try {
            val regex = new RegExp(FuzzingTaskProvider.vectIntToString(vec0))
            val automaton = new ByteRunAutomaton(regex.toAutomaton)
            val bytes = vec1.map { case IntValue(i) => i.toByte }.toArray
            BenchmarkSet.measureCost {
              automaton.run(bytes, 0, bytes.length)
            }
          }
          catch {
            case _: TooComplexToDeterminizeException => 0
          }
      }
    )
  }

  def standardTokenizer: ProblemConfig = {
    ProblemConfig(
      "lucene.standardTokenizer",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(v) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          import patbench.lucene.analysis.standard.StandardTokenizer
          try {
            val str = FuzzingTaskProvider.vectIntToString(vec)
            val tokenizer = new StandardTokenizer()
            tokenizer.setReader(new java.io.StringReader(str))
            tokenizer.reset()
            BenchmarkSet.measureCost {
              var tokenCount = 0
              while (tokenizer.incrementToken) tokenCount += 1
              tokenizer.end()
              tokenizer.close()
            }
          }
      }
    )
  }

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.fuzzProblem(
      standardTokenizer,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(200)), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(0, true)
  }
}
