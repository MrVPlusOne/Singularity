package singularity.benchmarks

import edu.utexas.stac.Cost
import singularity._
import singularity.Runner.RunnerConfig

import scala.util.Random


object BenchmarkSet {
  def measureCost[A](f: => A): Long = {
    Cost.reset()
    f
    Cost.read()
  }

  def handleIllegalArgumentException[A](default: A)(f: => A): A = {
    try{
      f
    }catch {
      case _: IllegalArgumentException => default
    }
  }

  def handleRuntimeException[A](default: A)(f: => A): A = {
    try{
      f
    }catch {
      case _: RuntimeException => default
    }
  }

  def runExample(seed: Int, problemConfig: ProblemConfig, useGUI: Boolean, size: Int): Unit = {
    val rand = new Random(seed)
    Supernova.standardSupernova.fuzzProblem(
      problemConfig,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(size)), rand)
  }
}
