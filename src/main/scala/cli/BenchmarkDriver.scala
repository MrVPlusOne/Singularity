package cli

import patsyn.Runner.RunConfig
import patsyn._
import scopt.OptionParser
import visual.PatternPlot

object BenchmarkDriver {

  def getWorkingDir(opt: CliOption): String = {
    FileInteraction.getWorkingDir(opt.ioId)
  }

  def benchmarks(opt: CliOption): Map[String, FuzzingTaskProvider] = {
    import FuzzingTaskProvider._
    Map[String, FuzzingTaskProvider](
      // These benchmarks have already been solved
      "slowfuzz/insertionSort" -> insertionSortExample,
      "slowfuzz/quickSort" -> quickSortExample,
      "slowfuzz/phpHash" -> phpHashCollisionExample,
      "slowfuzz/regex1" -> slowFuzzRegexExample1,
      "slowfuzz/regex2" -> slowFuzzRegexExample2,
      "slowfuzz/regex3" -> slowFuzzRegexExample3,
      "stac/graphAnalyzer" -> graphAnalyzerExample(getWorkingDir(opt)),
      "stac/blogger" -> bloggerExample(opt.ioId),
      "stac/imageProcessor" -> imageExample(10, 10, getWorkingDir(opt)),
      "stac/linearAlgebra" -> linearAlgebraExample(10, getWorkingDir(opt)),
      "stac/airplan1" -> airplan1Example(getWorkingDir(opt)),
      "stac/airplan2" -> airplan2Example(getWorkingDir(opt)),
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
      "stac/textCrunchr" -> textCrunchrExample(getWorkingDir(opt)),
      "stac/gabfeed4" -> gabfeed4Example(getWorkingDir(opt)),
      "stac/airplan3" -> airplan3Example(getWorkingDir(opt)),
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
      "ds/pushRelabel" -> pushRelabelExample,

      // We cheated in these benchmark and therefore it is debatable whether to include them
      "stac/airplan5" -> airplan5Example(getWorkingDir(opt)),
    )
  }

  val benchmarkNames: Seq[String] = {
    val names = benchmarks(CliOption()).keys
    require(names.size == names.toSet.size, "benchmark name collision detected!")
    names.toSeq
  }

  def getBenchmarksFromTarget(target: String, cliOption: CliOption): Seq[(String, FuzzingTaskProvider)] = {
    target match {
      case "all" => benchmarks(cliOption).toSeq
      case name => Seq(name -> benchmarks(cliOption).getOrElse(name,
        throw new IllegalArgumentException("Cannot find benchmark named " + name)))
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
    val parser = cliOptParser()

    parser.parse(args, CliOption()).foreach {
      cliOption =>
        cliOption.extrapolatePattern match {
          case None =>
            cliOption.plotPattern match {
              case None =>
                val benchs = getBenchmarksFromTarget(cliOption.target, cliOption)
                val config = getRunConfig(cliOption)
                benchs.foreach { case (name, bench) =>
                  println(s"*** Task $name started ***")
                  try {
                    Runner.runExample(bench, cliOption.ioId, cliOption.seeds, config, useGUI = !cliOption.disableGui)
                    println(s"*** Task $name finished ***")

                  } catch {
                    case ex: Exception =>
                      System.err.println(s"Exception thrown: $ex")
                      ex.printStackTrace(System.err)
                      System.err.println(s"Task $name aborted. Continuing to the next task...")
                  }
                }
              case Some(plotArg) =>
                val taskProvider = benchmarks(cliOption).getOrElse(cliOption.target,
                  throw new IllegalArgumentException("Cannot find benchmark named " + cliOption.target))
                val ind = FileInteraction.readObjectFromFile[MultiStateInd](plotArg.indPath)
                PatternPlot.showResourceUsageChart(taskProvider, ind, plotArg.sizeLimit, plotArg.density)
            }
          case Some(extraArg) =>
            val ind = FileInteraction.readObjectFromFile[MultiStateInd](extraArg.indPath)
            val taskProvider = benchmarks(cliOption).getOrElse(cliOption.target,
              throw new IllegalArgumentException("Cannot find benchmark named " + cliOption.target))
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

      opt[Seq[Int]]('s', "seeds").valueName("<seed1>,<seed2>,...").action((ss, c) =>
        c.copy(seeds = ss)).text("The random seeds to use. Default to {0} (only 1 seed).")

      opt[Int]("population-size").action((x, c) =>
        c.copy(tournamentSize = x)).text("[GP parameter] Population size. Default to 500.")

      opt[Int]("tournament-size").hidden().action((x, c) =>
        c.copy(populationSize = x)).text("[GP parameter] Tournament size. Default to 7.")

      opt[Int]("evaluation-trials").hidden().action((x, c) =>
        c.copy(evaluationTrials = x)).text("[GP parameter] Number of evaluation trials. Default to 3.")

      opt[Int]("total-size-tolerance").hidden().action((x, c) =>
        c.copy(totalSizeTolerance = x)).text("[GP parameter] Total size tolerance. Default to 50.")

      opt[Int]("single-size-tolerance").hidden().action((x, c) =>
        c.copy(singleSizeTolerance = x)).text("[GP parameter] Single size tolerance. Default to 30.")

      opt[Int]("thread-num").hidden().action((x, c) =>
        c.copy(threadNum = x)).text("[GP parameter UNUSED] Thread number. Default to 1.")

      opt[Int]("time-limit").action((x, c) =>
        c.copy(timeLimitInMillis = x)).text("Time limit for each black-box execution (in milliseconds). Default to " +
        "10000.")

      opt[Int]("max-nonincrease-gen").hidden().action((x, c) =>
        c.copy(maxNonIncreaseTime = x)).text("[GP parameter] Stop after this number of generations if the fitness " +
        " for the best individual does not increase. Default to 150.")

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
