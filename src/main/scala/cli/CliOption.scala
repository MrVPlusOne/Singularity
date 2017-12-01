package cli

case class CliOption(target: String = "",
                     ioId: Int = 0,
                     seed: Int = 0,
                     disableGui: Boolean = false,
                     useSledgehammer: Boolean = true,
                     keepBestIndividuals: Boolean = false,
                     sizeOfInterestOverride: Option[Int] = None,
                     populationSize: Int = 500,
                     tournamentSize: Int = 7,
                     evaluationTrials: Int = 3,
                     totalSizeTolerance: Int = 50,
                     singleSizeTolerance: Int = 30,
                     threadNum: Int = 1,
                     timeLimitInMillis: Int = 10000,
                     maxNonIncreaseTime: Int = 150,
                     extrapolatePattern: Option[ExtrapolationArgs] = None,
                     plotPattern: Option[PlotArgs] = None
                    )


case class ExtrapolationArgs(indPath: String, outputName: String, size: Int, memoryLimit: Long, evaluatePerformance: Boolean)

case class PlotArgs(indPath: String, sizeLimit: Int, density: Int)