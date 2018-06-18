package singularity

import singularity.Runner.RunnerConfig

import scala.util.Random

case class ProblemConfig(problemName: String,
                         outputTypes: IS[EType],
                         resourceUsage: IS[EValue] => Double,
                         sizeF: IS[EValue] => Int = _.map(_.memoryUsage.toInt).sum,
                         displayValue: IS[EValue] => String = FuzzingTaskProvider.defaultDisplayValue,
                         saveValueWithName: (IS[EValue], String) => Unit = FuzzingTaskProvider.defaultSaveValueWithName
                        )

object ProblemConfig{

}

case class GPConfig(populationSize: Int = 500,
                    tournamentSize: Int = 7,
                    totalSizeTolerance: Int = 60,
                    singleSizeTolerance: Int = 30,
                    exprCostPenaltyBase: Double = 0.95,
                    crossoverP: Double = 0.4,
                    mutateP: Double = 0.5,
                    copyP: Double = 0.05,
                    constFoldP: Double = 0.05
                   ){
  def show: String = {
    s"""
       |populationSize: $populationSize
       |tournamentSize: $tournamentSize
       |totalSizeTolerance: $totalSizeTolerance
       |singleSizeTolerance: $singleSizeTolerance
       |exprCostPenaltyBase: $exprCostPenaltyBase,
       |crossoverP: $crossoverP
       |mutateP: $mutateP
       |copyP: $copyP
       |constFoldP: $constFoldP
     """.stripMargin
  }
}

sealed trait EvalSizePolicy{
  def genSize(random: Random): Int

  def referenceSize: Int
}

case class FixedEvalSize(sizeOfInterest: Int) extends EvalSizePolicy{
  def genSize(random: Random) = sizeOfInterest

  def referenceSize: Int = sizeOfInterest
}

case class VariedEvalSize(referenceSize: Int, f: Random => Int, display: String) extends EvalSizePolicy{
  def genSize(random: Random) = f(random)

  override def toString = s"VariedEvalSize: $display"
}

object VariedEvalSize{
  def choppedGaussian(rand: Random, baseSize: Int,
                      gaussianChop: (Double, Double) = (-2,2),
                      expBase: Double = 2, powerE: Double = 0.5): VariedEvalSize = {
    VariedEvalSize(
      baseSize,
      rand => {
      (SimpleMath.expChoppedGaussian(gaussianChop, expBase, powerE)(rand) * baseSize).toInt
    },
      s"baseSize = $baseSize, chopMargin = $gaussianChop, base = $expBase")
  }
}

sealed trait ResourceUsagePolicy

object ResourceUsagePolicy{
  case class SimpleEvaluationPolicy(windowSize: Int = 1) extends ResourceUsagePolicy

  /** fit a power law and evaluate usage at size = scaleFactor * sizeOfInterest */
  case class HybridEvaluationPolicy(minPointsToUse: Int = 8,
                                    maxPointsToUse: Int = 14,
                                    scaleFactor: Double = 10.0,
                                    maxIter: Int = 100) extends ResourceUsagePolicy

  /** fit a power law and returns the power as evaluation result */
  case class PowerLawEvaluationPolicy(minPointsToUse: Int = 8,
                                    maxPointsToUse: Int = 14,
                                    maxIter: Int = 100) extends ResourceUsagePolicy
}


case class ExecutionConfig(evalSizePolicy: EvalSizePolicy = FixedEvalSize(300),
                           threadNum: Int = 1,
                           timeLimitInMillis: Int = 120000,
                           maxNonIncreaseGen: Option[Int] = Some(150),
                           maxFuzzingTimeSec: Option[Long] = None,
                           resourcePolicy: ResourceUsagePolicy = ResourceUsagePolicy.SimpleEvaluationPolicy()
                          ){
  def show: String = {
    s"""
       |sizeOfInterest: $evalSizePolicy
       |threadNum：$threadNum
       |timeLimitInMillis：$timeLimitInMillis
       |maxNonIncreaseGen：$maxNonIncreaseGen
       |maxFuzzingTimeSec: $maxFuzzingTimeSec
       |resourcePolicy: $resourcePolicy
     """.stripMargin
  }
}

case class RunConfig(runnerConfig: RunnerConfig,
                     gpConfig: GPConfig,
                     execConfig: ExecutionConfig) {

def withIoIdAndSeed(ioId: Int, seed: Int): RunConfig = {
    this.copy(
      runnerConfig = runnerConfig.copy(ioId = ioId, randomSeed = seed)
    )
  }

  def show: String = {
    s"""
       |[Runner config]
       |${runnerConfig.show}
       |[GP config]
       |${gpConfig.show}
       |[execution config]
       |${execConfig.show}
       """.stripMargin
  }
}

object RunConfig{
  def default = RunConfig(RunnerConfig(), GPConfig(), ExecutionConfig())
}
