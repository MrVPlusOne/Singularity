package cli

case class CliOption(target: String = "",
                     ioId: Int = 0,
                     seeds: Seq[Int] = Seq(0),
                     disableGui: Boolean = false,
                     extrapolatePattern: Option[ExtrapolationArgs] = None
                    )


case class ExtrapolationArgs(indPath: String, outputName: String, size: Int)