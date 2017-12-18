package benchmarks

import java.util.Random

import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._

object LuceneExamples {
  import patbench.lucene.analysis.Analyzer
  def configFromAnalyzer(name: String, analyzer: Analyzer)= {
    ProblemConfig(
      s"lucene.analyzer.$name",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(v) =>
          v.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val str = FuzzingTaskProvider.vectIntToString(vec)
          BenchmarkSet.measureCost {
            TestLucene.tokenizeWithAnalyzer(analyzer, str)
          }
      }
    )
  }

  def standardAnalyzer = configFromAnalyzer("standard", new patbench.lucene.analysis.standard.StandardAnalyzer)
  def chineseAnalyzer = configFromAnalyzer("smartcn", new patbench.lucene.analysis.cn.smart.SmartChineseAnalyzer)
  def japaneseAnalyzer = configFromAnalyzer("kuromoji", new patbench.lucene.analysis.ja.JapaneseAnalyzer)
  def polishAnalyzer = configFromAnalyzer("polish", new patbench.lucene.analysis.pl.PolishAnalyzer)

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.fuzzProblem(
      chineseAnalyzer,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(200)), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(1, true)
  }
}
