package patsyn

import patsyn.Runner.RunnerConfig

import scala.util.Random

case class ProblemConfig(problemName: String,
                         outputTypes: IS[EType],
                         sizeF: IS[EValue] => Int,
                         resourceUsage: IS[EValue] => Double,
                         displayValue: IS[EValue] => String = FuzzingTaskProvider.defaultDisplayValue,
                         saveValueWithName: (IS[EValue], String) => Unit = FuzzingTaskProvider.defaultSaveValueWithName
                        )

object ProblemConfig{

}

case class GPConfig(populationSize: Int = 500,
                    tournamentSize: Int = 7,
                    evaluationTrials: Int = 1,
                    totalSizeTolerance: Int = 60,
                    singleSizeTolerance: Int = 30,
                    crossoverP: Double = 0.4,
                    mutateP: Double = 0.5,
                    copyP: Double = 0.1
                   ){
  def show: String = {
    s"""
       |populationSize: $populationSize
       |tournamentSize: $tournamentSize
       |evaluationTrials：$evaluationTrials
       |totalSizeTolerance：$totalSizeTolerance
       |singleSizeTolerance：$singleSizeTolerance
       |crossoverP: $crossoverP
       |mutateP: $mutateP
       |copyP: $copyP
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
  def choppedGaussian(rand: Random, baseSize: Int, gaussianChop: (Double, Double) = (-1,2), expBase: Double = 2): VariedEvalSize = {
    VariedEvalSize(
      baseSize,
      rand => {
      (SimpleMath.expChoppedGaussian(gaussianChop, expBase)(rand) * baseSize).toInt
    },
      s"baseSize = $baseSize, chopMargin = $gaussianChop, base = $expBase")
  }
}

case class ExecutionConfig(evalSizePolicy: EvalSizePolicy = FixedEvalSize(300),
                           threadNum: Int = 1,
                           timeLimitInMillis: Int = 120000,
                           maxNonIncreaseGen: Option[Int] = Some(150),
                           maxFuzzingTimeSec: Option[Long] = None
                          ){
  def show: String = {
    s"""
       |sizeOfInterest: $evalSizePolicy
       |threadNum：$threadNum
       |timeLimitInMillis：$timeLimitInMillis
       |maxNonIncreaseGen：$maxNonIncreaseGen
       |maxFuzzingTimeSec: $maxFuzzingTimeSec
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
