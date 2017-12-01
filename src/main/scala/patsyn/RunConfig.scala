package patsyn

case class BenchmarkConfig(ioId: Int = 0,
                           sizeOfInterest: Option[Int] = None)

case class GPConfig(populationSize: Int = 500,
                    tournamentSize: Int = 7,
                    evaluationTrials: Int = 1,
                    totalSizeTolerance: Int = 60,
                    singleSizeTolerance: Int = 30)

case class ExecutionConfig(threadNum: Int = 1,
                           timeLimitInMillis: Int = 120000,
                           maxNonIncreaseTime: Int = 150,
                           randomSeed: Int = 0,
                           useGUI: Boolean = true)

case class RunConfig(benchConfig: BenchmarkConfig,
                     gpConfig: GPConfig,
                     execConfig: ExecutionConfig) {

  def withIoIdAndSeed(ioId: Int, seed: Int): RunConfig = {
    this.copy(
      benchConfig = benchConfig.copy(ioId = ioId),
      execConfig = execConfig.copy(randomSeed = seed)
    )
  }

  def show: String = {
    s"""
       |ioId: ${benchConfig.ioId}
       |sizeOfInterestOverride: ${benchConfig.sizeOfInterest}
       |populationSize: ${gpConfig.populationSize}
       |tournamentSize: ${gpConfig.tournamentSize}
       |evaluationTrials：${gpConfig.evaluationTrials}
       |totalSizeTolerance：${gpConfig.totalSizeTolerance}
       |singleSizeTolerance：${gpConfig.singleSizeTolerance}
       |threadNum：${execConfig.threadNum}
       |timeLimitInMillis：${execConfig.timeLimitInMillis}
       |maxNonIncreaseTime：${execConfig.maxNonIncreaseTime}
       |randomSeed: ${execConfig.randomSeed}
       |useGUI: ${execConfig.useGUI}
       """.stripMargin
  }
}

object RunConfig{
  def default = RunConfig(BenchmarkConfig(), GPConfig(), ExecutionConfig())
}
