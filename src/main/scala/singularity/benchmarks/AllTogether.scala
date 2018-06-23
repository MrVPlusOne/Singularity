package singularity.benchmarks

import patbench.commons.math3.fitting.{GaussianCurveFitter, HarmonicCurveFitter, PolynomialCurveFitter}
import singularity.Runner.RunnerConfig
import singularity._

import scala.util.Random

object AllTogether {
  type ConfigGen = (Int, String) => ProblemConfig

  def toGen(pc: ProblemConfig): ConfigGen = (_,_) => pc

  def toGen(fPc: String => ProblemConfig): ConfigGen = (_, dir) => fPc(dir)

  def allProblems: IS[ConfigGen] = VavrExamples.all.map(toGen) ++ SlowfuzzExamples.allExceptRegex.map(toGen) ++
    IS(CommonsExamples.curveFittingProblem("polyFit5", PolynomialCurveFitter.create(5)),
      CommonsExamples.curveFittingProblem("harmonicFit", HarmonicCurveFitter.create()),
      CommonsExamples.curveFittingProblem("gaussianFit", GaussianCurveFitter.create())
    ).map(toGen) ++
    GuavaExamples.all.map(toGen) ++
    (JGraphTExamples.maxFlowProblems ++ JGraphTExamples.spanningTreeProblems.map(_._1) ++
      JGraphTExamples.coloringProblems ++ JGraphTExamples.simpleCyclesProblems).map(toGen)

  /** Regexes from SlowFuzz */
  def allProblems2: IS[ConfigGen] = SlowfuzzExamples.allRegexProblems.map(toGen)


  def main(args: Array[String]): Unit = {
    val problems = SlowfuzzExamples.allExceptRegex.map(toGen) //allProblems ++ allProblems2
    val epochNum = 20
    val taskNum = problems.length * epochNum
    val fuzzingTimeLimit = 2*3600

    val seedBase = (allProblems.length + VavrExamples.all.length) * epochNum

    SimpleMath.processMap(args, 0 until taskNum,
      processNum = 35,
      mainClass = this
    ){
      i =>
        val problemId = i % problems.length
        val ioId = i + seedBase
        val p = problems(problemId)(ioId, FileInteraction.getWorkingDir(ioId = ioId))
        val rand = new Random(ioId)
        Supernova.standardSupernova.fuzzProblem(p,
          RunnerConfig(ioId = ioId, randomSeed= ioId, useGUI = false, callExitAfterFinish = false),
          ExecutionConfig(evalSizePolicy = FixedEvalSize(100), maxNonIncreaseGen = Some(200),
            timeLimitInMillis = 80000, maxFuzzingTimeSec = Some(fuzzingTimeLimit)), rand)
    }
  }

}
