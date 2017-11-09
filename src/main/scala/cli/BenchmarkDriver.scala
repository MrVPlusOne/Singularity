package cli

import patsyn._

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
      "slowfuzz/phpHash" -> (_ => phpHashCollision),
      "stac/graphAnalyzer" -> (opt => graphAnalyzerExample(getWorkingDir(opt))),
      "stac/blogger" -> (_ => bloggerExample),
      "stac/imageProcessor" -> (opt => imageExample(10, 10, getWorkingDir(opt))),
      "stac/textCrunchr" -> (opt => textCrunchrExample(getWorkingDir(opt)))
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

      opt[Seq[String]]('e', "extrapolate").valueName("<indPath>,<outName>,<size>").
        action({ case (Seq(input, output, size), c) =>
        c.copy(extrapolatePattern = Some(ExtrapolationArgs(input, output, size.toInt)))
      }).
        text("Read a MultiStateIndividual from <indPath> and try to construct an input of size <size>, then save it using name <outName>")

      help("help").text("Prints this usage text")
      version("version").text("Prints the version info")

      override def showUsageOnError = true

      arg[String]("<target>").required().action( (x, c) =>
        c.copy(target=x)).text("Benchmark target to run.")

      note("\nBenchmark target list:\n" + benchmarks.keys.map(s => "  " + s).mkString("\n") + "\n")
    }

    parser.parse(args, CliOption()).foreach {
      cliOption =>
        val taskProvider = benchmarks.getOrElse(cliOption.target,
          throw new IllegalArgumentException("Cannot find benchmark named " + cliOption.target))(cliOption)
        cliOption.extrapolatePattern match {
          case None =>
            val benchs = getBenchmarks(cliOption.target, cliOption)
            benchs.foreach { case (name, bench) =>
              println(s"*** Task $name started ***")
              try {
                TestRun.runExample(bench, Seq(cliOption.seed), !cliOption.disableGui)
                println(s"*** Task $name finished ***")

              } catch {
                case ex: Exception =>
                  System.err.println(s"Exception thrown: $ex")
                  ex.printStackTrace(System.err)
                  System.err.println(s"Task $name aborted. Continuing to the next task...")
              }
            }
          case Some(extraArg) =>
            val ind = FileInteraction.readObjectFromFile[MultiStateInd](extraArg.indPath)
            saveExtrapolation(taskProvider, ind, extraArg.size, extraArg.outputName)
        }
    }
  }

  def saveExtrapolation(taskProvider: FuzzingTaskProvider, individual: MultiStateInd, size: Int, name: String): Unit = {
    println(s"Calculating extrapolation at size = $size ...")
    var lastSize = Int.MinValue
    val valueOfInterest = MultiStateRepresentation.individualToPattern(individual).map(_._2).takeWhile{
      value =>
        val newSize = taskProvider.sizeF(value)
        if(newSize <= lastSize){
          println("Warning: Can't reach specified size using this individual")
          false
        }else{
          lastSize = newSize
          newSize <= size
        }
    }.last

    println(s"Extrapolation calculated. Now save results to $name")
    taskProvider.saveValueWithName(valueOfInterest, name)
  }
}
