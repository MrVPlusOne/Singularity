package cli

import patsyn.Runner.RunConfig
import patsyn._

object BenchmarkDriver {

  def getWorkingDir(cliOption: CliOption): String = {
    val ioId = cliOption.ioId
    val workingDir = s"workingDir$ioId"
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
      "stac/textCrunchr" -> (opt => textCrunchrExample(getWorkingDir(opt))),
      "stac/linearAlgebra" -> (opt => linearAlgebraExample(10, getWorkingDir(opt))),
//      "stac/gabfeed2" -> (opt => gabFeed2Example(opt.ioId, getWorkingDir(opt)))
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

  def getRunConfig(option: CliOption): RunConfig = {
    RunConfig(
      populationSize = option.populationSize,
      tournamentSize = option.tournamentSize,
      evaluationTrials = option.evaluationTrials,
      totalSizeTolerance = option.totalSizeTolerance,
      singleSizeTolerance = option.singleSizeTolerance,
      threadNum = option.threadNum,
      timeLimitInMillis = option.timeLimitInMillis,
      maxNonIncreaseTime = option.maxNonIncreaseTime
    )
  }

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[CliOption]("patsyn_cli_driver") {
      head("patsyn Command Line Driver", "0.1")

      opt[Unit]('n', "no-gui").action( (_, c) =>
        c.copy(disableGui = true)).text("Disable the GUI panel.")

      opt[Int]('i', "ioId").action( (id, c) =>
        c.copy(ioId = id)
      ).text("The id used to perform IO actions. If you have n processes running at the same time, just set their idIo to 0 through n.")

      opt[Seq[Int]]('s', "seeds").valueName("<seed1>,<seed2>,...").action( (ss, c) =>
        c.copy(seeds=ss)).text("The random seeds to use. Default to {0} (only 1 seed).")

      opt[Int]('p', "population-size").action( (x, c) =>
        c.copy(tournamentSize = x)).text("[GP parameter] Population size. Default to 500.")

      opt[Int]("tournament-size").hidden().action( (x, c) =>
        c.copy(populationSize = x)).text("[GP parameter] Tournament size. Default to 7.")

      opt[Int]("evaluation-trials").hidden().action( (x, c) =>
        c.copy(evaluationTrials = x)).text("[GP parameter] Number of evaluation trials. Default to 3.")

      opt[Int]("total-size-tolerance").hidden().action( (x, c) =>
        c.copy(totalSizeTolerance = x)).text("[GP parameter] Total size tolerance. Default to 50.")

      opt[Int]("single-size-tolerance").hidden().action( (x, c) =>
        c.copy(singleSizeTolerance = x)).text("[GP parameter] Single size tolerance. Default to 30.")

      opt[Int]("thread-num").hidden().action( (x, c) =>
        c.copy(threadNum = x)).text("[GP parameter UNUSED] Thread number. Default to 1.")

      opt[Int]("time-limit").action( (x, c) =>
        c.copy(timeLimitInMillis = x)).text("Time limit for each black-box execution (in milliseconds). Default to " +
        "10000.")

      opt[Int]("max-nonincrease-gen").hidden().action( (x, c) =>
        c.copy(maxNonIncreaseTime = x)).text("[GP parameter] Stop after this number of generations if the fitness " +
        " for the best individual does not increase. Default to 150.")

      opt[Seq[String]]('e', "extrapolate").valueName("<indPath>,<outName>,<size>[,<memory>]").
        action({
          case (Seq(input, output, size), c) =>
            c.copy(extrapolatePattern = Some(ExtrapolationArgs(input, output, size.toInt, Long.MaxValue)))
          case (Seq(input, output, size, memory), c) =>
            c.copy(extrapolatePattern = Some(ExtrapolationArgs(input, output, size.toInt, memory.toLong)))
      }).
        text("Read a MultiStateIndividual from <indPath> and try to construct an input of size <size>, then save it using name <outName>. Optionally, you can specify a memory limit for large input construction.")

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
            val config = getRunConfig(cliOption)
            benchs.foreach { case (name, bench) =>
              println(s"*** Task $name started ***")
              try {
                Runner.runExample(bench, cliOption.seeds, config, useGUI = !cliOption.disableGui)
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
            saveExtrapolation(taskProvider, ind, extraArg.size, extraArg.memoryLimit, extraArg.outputName)
        }
    }
  }

  def saveExtrapolation(taskProvider: FuzzingTaskProvider, individual: MultiStateInd,
                        sizeLimit: Int, memoryLimit: Long, name: String): Unit = {
    import EvolutionRepresentation.MemoryUsage

    println(s"Calculating extrapolation at size = $sizeLimit ...")
    var lastSize = Int.MinValue
    var progress = 0

    val valueOfInterest = MultiStateRepresentation.individualToPattern(individual).takeWhile{
      case (MemoryUsage(memory), value) =>
        val newSize = taskProvider.sizeF(value)
        if(newSize <= lastSize){
          println("Warning: Can't reach specified size using this individual")
          false
        }else{
          progress += 1

          lastSize = newSize
          newSize <= sizeLimit && memory <= memoryLimit
        }
    }.last._2

    println(s"Extrapolation calculated. Now save results to $name")
    taskProvider.saveValueWithName(valueOfInterest, name)
  }
}
