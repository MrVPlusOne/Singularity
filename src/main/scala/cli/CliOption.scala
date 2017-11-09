package cli

case class CliOption(target: String = "",
                     seed: Int = 0,
                     disableGui: Boolean = false,
                     extrapolatePattern: Option[ExtrapolationArgs] = None
                    )


case class ExtrapolationArgs(indPath: String, outputName: String, size: Int)