package benchmarks

import edu.utexas.stac.Cost
import patsyn._
import patsyn.Runner.RunnerConfig

import scala.util.Random


object BenchmarkSet {
  def measureCost[A](f: => A): Long = {
    Cost.reset()
    f
    Cost.read()
  }

  def runExample(seed: Int, problemConfig: ProblemConfig, useGUI: Boolean, size: Int): Unit = {
    val rand = new Random(seed)
    Supernova.standardSupernova.fuzzProblem(
      problemConfig,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(size)), rand)
  }

  def main(args: Array[String]): Unit = {
    val problem = CommonsExamples.compressionProblem
    runExample(seed = 3, problem, useGUI = true, size = 400)
  }
}
