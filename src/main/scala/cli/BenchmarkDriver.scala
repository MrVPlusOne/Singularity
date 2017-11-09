package cli

import patsyn.{FileInteraction, FuzzingExample, TestRun}

object BenchmarkDriver {

  def getWorkingDir(cliOption: CliOption): String = {
    val seed = cliOption.seed
    val workingDir = s"workingDir$seed"
    FileInteraction.mkDirsAlongPath(workingDir)
    workingDir
  }

  val benchmarks: Map[String, CliOption => FuzzingExample] = {
    import FuzzingExample._
    Map[String, CliOption => FuzzingExample](
      "slowfuzz/insertionSort" -> (_ =>insertionSortExample),
      "slowfuzz/quickSort" -> (_ => quickSortExample),
      "stac/graphAnalyzer" -> (opt => graphAnalyzerExample(getWorkingDir(opt))),
      "stac/blogger" -> (_ => bloggerExample),
      "stac/imageProcessor" -> (opt => imageExample(10, 10, getWorkingDir(opt)))
    )
  }

  def benchmarkNames: Set[String] = benchmarks.keySet

  def getBenchmark(name: String, cliOption: CliOption): FuzzingExample = {
    benchmarks.getOrElse(name,
      throw new IllegalArgumentException("Cannot find benchmark named " + name)).apply(cliOption)
  }

  def getBenchmarks(target: String, cliOption: CliOption): Seq[FuzzingExample] = {
    target match {
      case "all" => benchmarks.values.toSeq.map(_.apply(cliOption))
      case name => Seq(getBenchmark(name, cliOption))
    }
  }

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[CliOption]("patsyn_cli_driver") {
      head("patsyn Command Line Driver", "0.1")

      opt[Unit]('n', "no-gui").action( (x, c) =>
        c.copy(disableGui = true)).text("Disable the GUI panel.")

      opt[Int]('s', "seed").action( (x, c) =>
        c.copy(seed=x)).text("The random seed to use. Default to 0.")

      help("help").text("Prints this usage text")
      version("version").text("Prints the version info")

      override def showUsageOnError = true

      arg[String]("<target>").required().action( (x, c) =>
        c.copy(target=x)).text("Benchmark target to run.")

      note("\nBenchmark target list:\n" + benchmarkNames.map(s => "  " + s).mkString("\n") + "\n")
    }

    parser.parse(args, CliOption()).foreach { cliOption =>
      val benchs = getBenchmarks(cliOption.target, cliOption)
      benchs.foreach(bench => TestRun.runExample(bench, cliOption.seed, !cliOption.disableGui))
    }
  }
}
