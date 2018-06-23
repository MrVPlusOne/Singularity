package singularity.cli

import singularity._
import scopt.OptionParser
import visual.PatternPlot
import scala.util.Random
import FuzzingTaskProvider._
import benchmarks.ExampleAlgorithms._
import benchmarks.ExampleAlgorithms.NativeSortExamples._

object BenchmarkDriver {

  private def benchmarks(ioId: Int): Map[String, FuzzingTaskProvider] = {


    Map[String, FuzzingTaskProvider](

      // Evaluation 1 of section 1
      "slowfuzz/isort" -> insertionSortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/qsort" -> slowfuzzQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/appleqsort" -> appleQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/bsdqsort" -> bsdQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/pgqsort" -> pgQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/gnuqsort" -> gnuQsortNativeExample(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/phphash" -> phpHashNativeExample(128)(FileInteraction.getWorkingDir(ioId)),
      "slowfuzz/bzip" -> bzipExample(FileInteraction.getWorkingDir(ioId))) ++
      (0 until 20).map(i =>
        s"slowfuzz/pcre_regex$i" -> regexNativeExample(i, 100)(FileInteraction.getWorkingDir(ioId))
      ).toMap ++
    // Evaluations not included in the paper
    Map[String, FuzzingTaskProvider](
      // These benchmarks have already been solved
      "slowfuzz/emu/insertionSort" -> insertionSortExample,
      "slowfuzz/emu/quickSort" -> quickSortExample,
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

      // These benchmarks are yet to be solved
      "hashcol/php" -> phpHashCollisionExample,
      "hashcol/java" -> javaHashCollisionExample,
      "hashcol/ruby" -> rubyHashCollisionExample,
      "hashcol/asp.net" -> aspDotNetHashCollisionExample,
      "hashcol/python" -> pythonHashCollisionExample,
      "hashcol/v8" -> v8HashCollisionExample,
      "hashcol/murmur2s0" -> murmur2HashCollisionExample,
      "stac/textCrunchr" -> textCrunchrExample(FileInteraction.getWorkingDir(ioId)),
      "stac/airplan3" -> airplan3Example(FileInteraction.getWorkingDir(ioId)),
      "hashperf/php" -> phpHashPerformanceExample,
      "hashperf/java" -> javaHashPerformanceExample,
      "hashperf/ruby" -> rubyHashPerformanceExample,
      "hashperf/asp.net" -> aspDotNetHashPerformanceExample,
      "hashperf/python" -> pythonHashPerformanceExample,
      "hashperf/v8" -> v8HashCollisionExample,
      "hashperf/murmur2s0" -> murmur2HashPerformanceExample
    )
  }

  val benchmarkNames: Seq[String] = {
    val names = benchmarks(ioId = 0).keys
    require(names.size == names.toSet.size, "benchmark name collision detected!")
    names.toSeq
  }


  def getRunConfig(option: CliOption, size: Int): AllConfigs = {
    import option._

    val execConfigOverride = execConfig.copy(evalSizePolicy = FixedEvalSize(size))

    AllConfigs(runnerConfig, gpConfig, execConfigOverride)
  }

  def main(args: Array[String]): Unit = {
    val parser = cliOptParser()

    parser.parse(args, CliOption()).foreach {
      cliOption =>
        val name = cliOption.target
        val ioId = cliOption.runnerConfig.ioId
        val taskProvider = benchmarks(ioId).getOrElse(name, throw new
            IllegalArgumentException("Cannot find benchmark named " + name))
        val random = new Random(cliOption.runnerConfig.ioId)

        taskProvider.run{ task =>
          val sizeOfInterest = cliOption.sizeOfInterestOverride.getOrElse(task.sizeOfInterest)
          val config = getRunConfig(cliOption, sizeOfInterest)

          cliOption.extrapolatePattern match {
            case None =>
              cliOption.plotPattern match {
                case None =>
                  println(s"*** Task $name started ***")
                  try {
                    if (cliOption.useSledgehammer) {
                      Supernova.standardSupernova.fuzzTask(name, taskProvider, config.runnerConfig, config.execConfig, random)
                    } else {
                      Runner.runExample(name, taskProvider, config)
                    }
                    println(s"*** Task $name finished ***")

                  } catch {
                    case ex: Exception =>
                      System.err.println(s"Exception thrown: $ex")
                      ex.printStackTrace(System.err)
                      System.err.println(s"Task $name aborted. Continuing to the next task...")
                  }

                case Some(plotArg) =>
                  val ind = FileInteraction.readMultiIndFromFile(plotArg.indPath, StandardSystem.funcMap)
                  taskProvider.runAsProbConfig(name) { config =>
                    PatternPlot.showResourceUsageChart(config, ind, plotArg.sizeLimit, plotArg.density)
                  }
              }
            case Some(extraArg) =>
              taskProvider.runAsProbConfig(name){ probConfig =>
                val ind = FileInteraction.readMultiIndFromFile(extraArg.indPath, StandardSystem.funcMap)
                MultiStateRepresentation.saveExtrapolation(
                  probConfig, ind, extraArg.size, extraArg.memoryLimit, extraArg.outputName, extraArg.evaluatePerformance)
              }

          }

        }

    }
  }

  def cliOptParser(): OptionParser[CliOption] = {
    new scopt.OptionParser[CliOption]("patsyn_cli_driver") {
      head("patsyn Command Line Driver", "0.1")

      opt[Unit]('n', "no-gui").action((_, c) =>
        c.copy(runnerConfig = c.runnerConfig.copy(useGUI = false))).text("Disable the GUI panel.")

      opt[Int]('i', "ioId").action((id, c) =>
        c.copy(runnerConfig = c.runnerConfig.copy(ioId = id))
      ).text("The id used to perform IO actions. If you have n processes running at the same time, just set their idIo to 0 through n.")

      opt[Int]('s', "seed").action((s, c) =>
        c.copy(runnerConfig = c.runnerConfig.copy(randomSeed = s))).text("The random seed to use. Default to 0.")

      opt[Unit]('m', "manual").action((_, c) =>
        c.copy(useSledgehammer = false)).text("Manually specify GP parameters instead of letting the tool " +
        "auto-configure them. This option is off by default.")

      opt[Int]("thread-num").action((x, c) =>
        c.copy(execConfig = c.execConfig.copy(threadNum = x))).text("Thread number to use for individual evaluation. Default to 1.")

      opt[Int]("max-nonincrease-gen").action((x, c) =>
        c.copy(execConfig = c.execConfig.copy(maxNonIncreaseGen = Some(x)))).text("Stop the program if the best fitness has not increased for this number of generations. Dose not specify this value means it should never stop.")

      opt[Int]("max-fuzzing-time").action((x, c) =>
        c.copy(execConfig = c.execConfig.copy(maxFuzzingTimeSec = Some(x)))).text("Stop the program after this amount of time (in seconds). Dose not specify this value means it should never stop.")

      opt[Int]("time-limit").action((x, c) =>
        c.copy(execConfig = c.execConfig.copy(timeLimitInMillis = x))).text("Time limit for each black-box execution (in milliseconds). Default to " +
        "120000.")

      opt[Unit]('k', "keep-best-individuals").action((_, c) =>
        c.copy(runnerConfig = c.runnerConfig.copy(keepBestIndividuals = true))).text("Each time the tool finds a better individual, preserve it in a " +
        "separated file instead of overwriting the file that stores the previous best one. Off by default.")

      opt[Int]("task-size").hidden().action((x, c) =>
        c.copy(sizeOfInterestOverride = Some(x))).text("Manually override the problem size for the fuzzing task. Only" +
        " specify this when you know what you are doing.")

      opt[Int]("population-size").hidden().action((x, c) =>
        c.copy(gpConfig = c.gpConfig.copy(populationSize = x))).text("[Only used when --manual is specified] Population size. Default to 500.")

      opt[Int]("tournament-size").hidden().action((x, c) =>
        c.copy(gpConfig = c.gpConfig.copy(tournamentSize = x))).text("[Only used when --manual is specified] Tournament size. Default to 7.")

      opt[Int]("total-size-tolerance").hidden().action((x, c) =>
        c.copy(gpConfig = c.gpConfig.copy(totalSizeTolerance = x))).text("[Only used when --manual is specified] Total size tolerance. Default to 50.")

      opt[Int]("single-size-tolerance").hidden().action((x, c) =>
        c.copy(gpConfig = c.gpConfig.copy(singleSizeTolerance = x))).text("[Only used when --manual is specified] Single size tolerance. Default to 30.")

      opt[Double]("crossover-p").hidden().action((x, c) =>
        c.copy(gpConfig = c.gpConfig.copy(crossoverP = x))).text("[Only used when --manual is specified] Crossover " +
        "probability. Default to 0.4.")

      opt[Double]("mutate-p").hidden().action((x, c) =>
        c.copy(gpConfig = c.gpConfig.copy(mutateP = x))).text("[Only used when --manual is specified] Mutation " +
        "probability. Default to 0.5.")

      opt[Double]("copy-p").hidden().action((x, c) =>
        c.copy(gpConfig = c.gpConfig.copy(copyP = x))).text("[Only used when --manual is specified] Copy " +
        "probability. Default to 0.1.")


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
