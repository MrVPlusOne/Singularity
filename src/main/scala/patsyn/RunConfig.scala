package patsyn

import patsyn.Runner.RunnerConfig

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

case class ExecutionConfig(sizeOfInterest: Int = 300,
                           threadNum: Int = 1,
                           timeLimitInMillis: Int = 120000,
                           maxNonIncreaseGen: Option[Int] = None,
                           maxFuzzingTimeSec: Option[Long] = None
                          ){
  def show: String = {
    s"""
       |sizeOfInterest: $sizeOfInterest
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
