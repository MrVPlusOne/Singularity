package cli

import patsyn.{ExecutionConfig, GPConfig}
import patsyn.Runner.RunnerConfig

case class CliOption(target: String = "",
                     useSledgehammer: Boolean = true,
                     runnerConfig: RunnerConfig = RunnerConfig(),
                     gpConfig: GPConfig = GPConfig(),
                     execConfig: ExecutionConfig = ExecutionConfig(),
                     sizeOfInterestOverride: Option[Int] = None,
                     extrapolatePattern: Option[ExtrapolationArgs] = None,
                     plotPattern: Option[PlotArgs] = None
                    )


case class ExtrapolationArgs(indPath: String, outputName: String, size: Int, memoryLimit: Long, evaluatePerformance: Boolean)

case class PlotArgs(indPath: String, sizeLimit: Int, density: Int)