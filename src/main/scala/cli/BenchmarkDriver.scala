package cli

import patsyn.{FileInteraction, FuzzingExample, TestRun}

object BenchmarkDriver {

  def getWorkingDir(cliOption: CliOption): String = {
    val seed = cliOption.seed
    val workingDir = s"workingDir$seed"
    FileInteraction.mkDirsAlongPath(workingDir)
    workingDir
  }

  def benchmarkNames = Seq(
    "slowfuzz/insertionsort",
    "slowfuzz/quicksort",
    "stac/graphanalyzer",
    "stac/blogger",
    "stac/imageprocessor"
  )

  // TODO: Make each benchmark take tunable parameters
  def getBenchmark(name: String, cliOption: CliOption): FuzzingExample = {
    name match {
      case "slowfuzz/insertionsort" =>
        FuzzingExample.insertionSortExample
      case "slowfuzz/quicksort" =>
        FuzzingExample.quickSortExample
      case "stac/graphanalyzer" =>
        FuzzingExample.graphAnalyzerExample(getWorkingDir(cliOption))
      case "stac/blogger" =>
        FuzzingExample.bloggerExample
      case "stac/imageprocessor" =>
        FuzzingExample.imageExample(10, 10, getWorkingDir(cliOption))
      case _ => throw new IllegalArgumentException("Cannot find benchmark named " + name)
    }
  }

  def getBenchmarks(target: String, cliOption: CliOption): Seq[FuzzingExample] = {
    target match {
      case "all" => benchmarkNames.map(name => getBenchmark(name, cliOption))
      case _@name => Seq(getBenchmark(name, cliOption))
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

    parser.parse(args, CliOption()) match {
      case Some(cliOption) =>
        val benchs = getBenchmarks(cliOption.target, cliOption)
        benchs.foreach(bench => TestRun.runExample(bench, cliOption.seed, !cliOption.disableGui))

      case None =>
        // do nothing
    }
  }
}
