package cli

import patsyn.{FileInteraction, FuzzingTask, FuzzingTaskProvider, TestRun}

object BenchmarkDriver {

  def getWorkingDir(cliOption: CliOption): String = {
    val seed = cliOption.seed
    val workingDir = s"workingDir$seed"
    FileInteraction.mkDirsAlongPath(workingDir)
    workingDir
  }

  val benchmarks = {
    import FuzzingTaskProvider._
    Map[String, CliOption => FuzzingTaskProvider](
      "slowfuzz/insertionSort" -> (_ =>insertionSortExample),
      "slowfuzz/quickSort" -> (_ => quickSortExample),
      "stac/graphAnalyzer" -> (opt => graphAnalyzerExample(getWorkingDir(opt))),
      "stac/blogger" -> (_ => bloggerExample),
      "stac/imageProcessor" -> (opt => imageExample(10, 10, getWorkingDir(opt)))
    )
  }

  def getBenchmarks(target: String, cliOption: CliOption): Iterator[(String, FuzzingTaskProvider)] = {
    target match {
      case "all" => benchmarks.toIterator.map{
        case (name, p) => name -> p(cliOption)
      }
      case name => Iterator(name -> benchmarks.getOrElse(name,
        throw new IllegalArgumentException("Cannot find benchmark named " + name)).apply(cliOption))
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

      note("\nBenchmark target list:\n" + benchmarks.keys.map(s => "  " + s).mkString("\n") + "\n")
    }

    parser.parse(args, CliOption()).foreach { cliOption =>
      val benchs = getBenchmarks(cliOption.target, cliOption)
      benchs.foreach{ case (name, bench) =>
        println(s"*** Task $name started ***")
        try {
          bench.run { task =>
            TestRun.runExample(task, cliOption.seed, !cliOption.disableGui)
            println(s"*** Task $name finished ***")
          }
        } catch {
          case ex: Exception =>
            System.err.println(s"Exception thrown: $ex")
            ex.printStackTrace(System.err)
            System.err.println(s"Task $name aborted. Continuing to the next task...")
        }
      }
    }
  }
}
