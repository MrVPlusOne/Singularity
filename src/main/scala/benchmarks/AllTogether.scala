package benchmarks

import patbench.commons.math3.fitting.{GaussianCurveFitter, HarmonicCurveFitter, PolynomialCurveFitter}
import patsyn.Runner.RunnerConfig
import patsyn._

import scala.util.Random

object AllTogether {
  type ConfigGen = String => ProblemConfig

  def toGen(pc: ProblemConfig): ConfigGen = _ => pc

  def allProblems: IS[ConfigGen] = VavrExamples.all.map(toGen) ++ SlowfuzzExamples.allExceptRegex ++
    IS(CommonsExamples.curveFittingProblem("polyFit5", PolynomialCurveFitter.create(5)),
      CommonsExamples.curveFittingProblem("harmonicFit", HarmonicCurveFitter.create()),
      CommonsExamples.curveFittingProblem("gaussianFit", GaussianCurveFitter.create())
    ).map(toGen) ++
    IS(DemanglerExamples.llvmDemanglerExample, DemanglerExamples.lldbDemanglerExample) ++
    GuavaExamples.all.map(toGen) ++
    (JGraphTExamples.maxFlowProblems ++ JGraphTExamples.spanningTreeProblems.map(_._1) ++
      JGraphTExamples.coloringProblems ++ JGraphTExamples.simpleCyclesProblems).map(toGen)

  def main(args: Array[String]): Unit = {
    val problems = allProblems
    val epochNum = 20
    val taskNum = problems.length * epochNum
    val fuzzingTimeLimit = 2*3600

    SimpleMath.processMap(args, 0 until taskNum,
      processNum = 35,
      mainClass = this
    ){
      i =>
        val problemId = i % problems.length
        val ioId = i
        val p = problems(problemId)(FileInteraction.getWorkingDir(ioId = ioId))
        val rand = new Random(ioId)
        Supernova.standardSupernova.fuzzProblem(p,
          RunnerConfig(ioId = ioId, randomSeed= ioId, useGUI = false, callExitAfterFinish = false),
          ExecutionConfig(evalSizePolicy = FixedEvalSize(100), maxNonIncreaseGen = Some(200),
            timeLimitInMillis = 80000, maxFuzzingTimeSec = Some(fuzzingTimeLimit)), rand)
    }
  }

}
