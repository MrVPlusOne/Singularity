package cli

import patsyn._
import scopt.OptionParser
import visual.PatternPlot

object BenchmarkDriver {

  private def benchmarks(benchConfig: BenchmarkConfig): Map[String, FuzzingTaskProvider] = {
    import FuzzingTaskProvider._
    val BenchmarkConfig(ioId, _) = benchConfig
    Map[String, FuzzingTaskProvider](

      // Evaluation section 1
      "slowfuzz/insertionSort" -> insertionSortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/qsort" -> slowfuzzQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/appleqsort" -> appleQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/bsdqsort" -> bsdQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/pgqsort" -> pgQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/phpHash" -> phpHashNativeExample(FileInteraction.getWorkingDir(ioId)),
      // TODO: slowfuzz/bzip and slowfuzz/regexes

      // These benchmarks have already been solved
      "slowfuzz/emu/insertionSort" -> insertionSortExample,
      "slowfuzz/emu/quickSort" -> quickSortExample,
      "slowfuzz/emu/phpHash" -> phpHashCollisionExample,
      "slowfuzz/emu/regex1" -> slowFuzzRegexExample1,
      "slowfuzz/emu/regex2" -> slowFuzzRegexExample2,
      "slowfuzz/emu/regex3" -> slowFuzzRegexExample3,
      "stac/graphAnalyzer" -> graphAnalyzerExample(FileInteraction.getWorkingDir(ioId)),
      "stac/blogger" -> bloggerExample(ioId),
      "stac/imageProcessor" -> imageExample(10, 10, FileInteraction.getWorkingDir(ioId)),
      "stac/linearAlgebra" -> linearAlgebraExample(10, FileInteraction.getWorkingDir(ioId)),
      "stac/airplan1" -> airplan1Example(FileInteraction.getWorkingDir(ioId)),
      "stac/airplan2" -> airplan2Example(FileInteraction.getWorkingDir(ioId)),
      "redos/cookie" -> regexExample("^(([^=;]+))\\s*=\\s*([^\\n\\r\\00]*)", defaultRegexDic),
      "quicksortMiddle" -> quickSortMiddlePivotExample,
      "hashperf/php" -> phpHashPerformanceExample,

      // These benchmarks are yet to be solved
      "hashcol/java" -> javaHashCollisionExample,
      "hashcol/ruby" -> rubyHashCollisionExample,
      "hashcol/asp.net" -> aspDotNetHashCollisionExample,
      "hashcol/python" -> pythonHashCollisionExample,
      "hashcol/v8" -> v8HashCollisionExample,
      "hashcol/murmur2s0" -> murmur2HashCollisionExample,
      "stac/textCrunchr" -> textCrunchrExample(FileInteraction.getWorkingDir(ioId)),
      "stac/gabfeed4" -> gabfeed4Example(FileInteraction.getWorkingDir(ioId)),
      "stac/airplan3" -> airplan3Example(FileInteraction.getWorkingDir(ioId)),
      "hashperf/java" -> javaHashPerformanceExample,
      "hashperf/ruby" -> rubyHashPerformanceExample,
      "hashperf/asp.net" -> aspDotNetHashPerformanceExample,
      "hashperf/python" -> pythonHashPerformanceExample,
      "hashperf/v8" -> v8HashCollisionExample,
      "hashperf/murmur2s0" -> murmur2HashPerformanceExample,
      "ds/splaytree" -> splayTreeExample,
      "ds/fordFulkersonDFS" -> fordFulkersonExample(false),
      "ds/fordFulkersonBFS" -> fordFulkersonExample(true),
      "ds/dinic" -> dinicExample,
      "ds/pushRelabel" -> pushRelabelExample
    )
  }

  val benchmarkNames: Seq[String] = {
    val names = benchmarks(BenchmarkConfig()).keys
    require(names.size == names.toSet.size, "benchmark name collision detected!")
    names.toSeq
  }

  def getBenchmarkFromTarget(target: String, benchConfig: BenchmarkConfig): Option[FuzzingTaskProvider] = {
    benchmarks(benchConfig).get(target)
  }

  def getRunConfig(option: CliOption): RunConfig = {
    val benchConfig = BenchmarkConfig(option.ioId, option.sizeOfInterestOverride)
    val gpConfig = GPConfig(option.populationSize,
                            option.tournamentSize,
                            option.evaluationTrials,
                            option.totalSizeTolerance,
                            option.singleSizeTolerance)
    val execConfig = ExecutionConfig(option.threadNum,
                                     option.timeLimitInMillis,
                                     option.maxNonIncreaseTime,
                                     option.seed,
                                     !option.disableGui)
    RunConfig(benchConfig, gpConfig, execConfig)
  }

  def main(args: Array[String]): Unit = {
    val parser = cliOptParser()

    parser.parse(args, CliOption()).foreach {
      cliOption =>
        val config = getRunConfig(cliOption)
        val name = cliOption.target
        val taskProvider = getBenchmarkFromTarget(name, config.benchConfig).getOrElse(throw new
            IllegalArgumentException("Cannot find benchmark named " + name))

        cliOption.extrapolatePattern match {
          case None =>
            cliOption.plotPattern match {
              case None =>
                println(s"*** Task $name started ***")
                try {
                  if (cliOption.useSledgehammer) {
                    Sledgehammer.sledgehammerRun(taskProvider, config)
                  } else {
                    Runner.runExample(taskProvider, config)
                  }
                  println(s"*** Task $name finished ***")

                } catch {
                  case ex: Exception =>
                    System.err.println(s"Exception thrown: $ex")
                    ex.printStackTrace(System.err)
                    System.err.println(s"Task $name aborted. Continuing to the next task...")
                }

              case Some(plotArg) =>
                val ind = FileInteraction.readObjectFromFile[MultiStateInd](plotArg.indPath)
                PatternPlot.showResourceUsageChart(taskProvider, ind, plotArg.sizeLimit, plotArg.density)
            }
          case Some(extraArg) =>
            val ind = FileInteraction.readObjectFromFile[MultiStateInd](extraArg.indPath)
            MultiStateRepresentation.saveExtrapolation(
              taskProvider, ind, extraArg.size, extraArg.memoryLimit, extraArg.outputName, extraArg.evaluatePerformance)
        }
    }
  }

  def cliOptParser(): OptionParser[CliOption] = {
    new scopt.OptionParser[CliOption]("patsyn_cli_driver") {
      head("patsyn Command Line Driver", "0.1")

      opt[Unit]('n', "no-gui").action((_, c) =>
        c.copy(disableGui = true)).text("Disable the GUI panel.")

      opt[Int]('i', "ioId").action((id, c) =>
        c.copy(ioId = id)
      ).text("The id used to perform IO actions. If you have n processes running at the same time, just set their idIo to 0 through n.")

      opt[Int]('s', "seed").action((s, c) =>
        c.copy(seed = s)).text("The random seed to use. Default to 0.")

      opt[Unit]('m', "manual").action((_, c) =>
        c.copy(useSledgehammer = false)).text("Manually specify GP parameters instead of letting the tool " +
        "auto-configure them. This option is off by default.")

      opt[Int]("size-override").hidden().action((x, c) =>
        c.copy(sizeOfInterestOverride = Some(x))).text("Manually override the problem size for evaluation. Only " +
        "specify this when you know what you are doing.")

      opt[Int]("thread-num").action((x, c) =>
        c.copy(threadNum = x)).text("Thread number to use for individual evaluation. Default to 1.")

      opt[Int]("max-nonincrease-gen").action((x, c) =>
        c.copy(maxNonIncreaseTime = x)).text("Stop after this number of generations if the fitness " +
        " for the best individual does not increase. Default to 150.")

      opt[Int]("time-limit").action((x, c) =>
        c.copy(timeLimitInMillis = x)).text("Time limit for each black-box execution (in milliseconds). Default to " +
        "10000.")

      opt[Int]("population-size").hidden().action((x, c) =>
        c.copy(tournamentSize = x)).text("[GP parameter] Population size. Default to 500.")

      opt[Int]("tournament-size").hidden().action((x, c) =>
        c.copy(populationSize = x)).text("[GP parameter] Tournament size. Default to 7.")

      opt[Int]("evaluation-trials").hidden().action((x, c) =>
        c.copy(evaluationTrials = x)).text("[GP parameter] Number of evaluation trials. Default to 3.")

      opt[Int]("total-size-tolerance").hidden().action((x, c) =>
        c.copy(totalSizeTolerance = x)).text("[GP parameter] Total size tolerance. Default to 50.")

      opt[Int]("single-size-tolerance").hidden().action((x, c) =>
        c.copy(singleSizeTolerance = x)).text("[GP parameter] Single size tolerance. Default to 30.")


      def parseBool(s: String): Boolean = {
        Set("t", "true", "yes").contains(s.trim.toLowerCase())
      }

      opt[Seq[String]]('e', "extrapolate").valueName("<indPath>,<outName>,<size>,<eval>[,<memory>]").
        action({
          case (Seq(input, output, size, eval), c) =>
            val shouldEval = parseBool(eval)
            c.copy(extrapolatePattern = Some(ExtrapolationArgs(input, output, size.toInt, Long.MaxValue, shouldEval)))
          case (Seq(input, output, size, eval, memory), c) =>
            val shouldEval = parseBool(eval)
            c.copy(extrapolatePattern = Some(ExtrapolationArgs(input, output, size.toInt, memory.toLong, shouldEval)))
        }).
        text("Read a MultiStateIndividual from <indPath> and try to construct an input of size <size>, then save it using name <outName>. If <eval> = \"t\", the extrapolated value is also evaluated. Optionally, you can specify a memory limit for large input construction.")

      opt[Seq[String]]('p', "plot").valueName("<indPath>,<sizeLimit>,[,<density>]").
        action({
          case (Seq(input, size), c) =>
            c.copy(plotPattern = Some(PlotArgs(input, size.toInt, 10)))
          case (Seq(input, size, density), c) =>
            c.copy(plotPattern = Some(PlotArgs(input, size.toInt, density.toInt)))
        }).
        text("Read a MultiStateIndividual from <indPath> and try to plot a graph showing the resource usage from inputs of size 0 to <sizeLimit>. The optional <density> parameter controls how many points will be plotted.")

      help("help").text("Prints this usage text")
      version("version").text("Prints the version info")

      override def showUsageOnError = true

      arg[String]("<target>").required().action((x, c) =>
        c.copy(target = x)).text("Benchmark target to run.")

      note("\nBenchmark target list:\n" + benchmarkNames.map(s => "  " + s).mkString("\n") + "\n")
    }
  }


}
