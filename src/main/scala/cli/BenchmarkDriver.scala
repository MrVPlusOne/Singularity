package cli

import patsyn.Runner.RunConfig
import patsyn._
import scopt.OptionParser

object BenchmarkDriver {

  def getWorkingDir(opt: CliOption): String = {
    FileInteraction.getWorkingDir(opt.ioId)
  }

  private val SLOWFUZZ_REGEX1 = "(?i:(j|(&#x?0*((74)|(4A)|(106)|(6A));?))\n([\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n" +
    "(tab;)|(newline;))))*(a|(&#x?0*((65)|\n(41)|(97)|(61));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))\n))*(v|(&#x?0*((86)|(56)|(118)|(76));?)\n)([\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n(tab;)|(newline;))))*(a|(&#x?0*((65)|\n(41)|(97)|(61));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))))*\n(s|(&#x?0*((83)|(53)|(115)|(73));?))(\n[\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n(tab;)|(newline;))))*(c|(&#x?0*((67)|\n(43)|(99)|(63));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))))*\n(r|(&#x?0*((82)|(52)|(114)|(72));?))\n([\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n(tab;)|(newline;))))*(i|(&#x?0*((73)|\n(49)|(105)|(69));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))))*\n(p|(&#x?0*((80)|(50)|(112)|(70));?))\n([\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n(tab;)|(newline;))))*(t|(&#x?0*((84)|\n(54)|(116)|(74));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))))\n*(:|(&((#x?0*((58)|(3A));?)|(colon;)\n))).)"
  private val SLOWFUZZ_REGEX2 = "<(a|abbr|acronym|address|applet|area|\naudioscope|b|base|basefront|bdo|\nbgsound|big" +
    "|blackface|blink|\nblockquote|body|bq|br|button|caption|\ncenter|cite|code|col|colgroup|comment|dd|del|dfn|dir|div|dl|\ndt|em|embed|fieldset|fn|font|\nform|frame|frameset|h1|head|hr|\nhtml|i|iframe|ilayer|img|input|ins|\nisindex|kdb|keygen|label|layer|\nlegend|li|limittext|link|listing|\nmap|marquee|menu|meta|multicol|\nnobr|noembed|noframes|noscript|\nnosmartquotes|object|ol|optgroup|\noption|p|param|plaintext|pre|q|\nrt|ruby|s|samp|script|select|\nserver|shadow|sidebar|small|\nspacer|span|strike|strong|style|\nsub|sup|table|tbody|td|textarea|\ntfoot|th|thead|title|tr|tt|u|ul|\nvar|wbr|xml|xmp)\\\\W"
  private val SLOWFUZZ_REGEX3 = "(?i:<.*[:]vmlframe.*?[ /+\\t]*?src[\n/+\\t]*=)"

  val benchmarks = {
    import FuzzingTaskProvider._
    Map[String, CliOption => FuzzingTaskProvider](
      // These benchmarks have already been solved
      "slowfuzz/insertionSort" -> (_ => insertionSortExample),
      "slowfuzz/quickSort" -> (_ => quickSortExample),
      "slowfuzz/phpHash" -> (_ => phpHashCollisionExample),
      "slowfuzz/regex1" -> (_ => regexExample(SLOWFUZZ_REGEX1, defaultRegexDic)),
      "slowfuzz/regex2" -> (_ => regexExample(SLOWFUZZ_REGEX2, defaultRegexDic)),
      "slowfuzz/regex3" -> (_ => regexExample(SLOWFUZZ_REGEX3, defaultRegexDic)),
      "stac/graphAnalyzer" -> (opt => graphAnalyzerExample(getWorkingDir(opt))),
      "stac/blogger" -> (opt => bloggerExample(opt.ioId)),
      "stac/imageProcessor" -> (opt => imageExample(10, 10, getWorkingDir(opt))),
      "stac/linearAlgebra" -> (opt => linearAlgebraExample(10, getWorkingDir(opt))),
      "stac/airplan1" -> (opt => airplan1Example(getWorkingDir(opt))),
      "redos/cookie" -> (_ => regexExample("^(([^=;]+))\\s*=\\s*([^\\n\\r\\00]*)", defaultRegexDic)),
      "hashperf/php" -> (_ => phpHashPerformanceExample),

      // These benchmarks are yet to be solved
      "hashcol/java" -> (_ => javaHashCollisionExample),
      "hashcol/ruby" -> (_ => rubyHashCollisionExample),
      "hashcol/asp.net" -> (_ => aspDotNetHashCollisionExample),
      "hashcol/python" -> (_ => pythonHashCollisionExample),
      "hashcol/v8" -> (_ => v8HashCollisionExample),
      "hashcol/murmur2s0" -> (_ => murmur2HashCollisionExample),
      "stac/textCrunchr" -> (opt => textCrunchrExample(getWorkingDir(opt))),
      "stac/gabfeed4" -> (opt => gabfeed4Example(getWorkingDir(opt))),
      "stac/airplan2" -> (opt => airplan2Example(getWorkingDir(opt))),
      "stac/airplan3" -> (opt => airplan3Example(getWorkingDir(opt))),
      "hashperf/java" -> (_ => javaHashPerformanceExample),
      "hashperf/ruby" -> (_ => rubyHashPerformanceExample),
      "hashperf/asp.net" -> (_ => aspDotNetHashPerformanceExample),
      "hashperf/python" -> (_ => pythonHashPerformanceExample),
      "hashperf/v8" -> (_ => v8HashCollisionExample),
      "hashperf/murmur2s0" -> (_ => murmur2HashPerformanceExample),
      "ds/splaytree" -> (_ => splayTreeExample),

      // We cheated in these benchmark and therefore it is debatable whether to include them
      "stac/airplan5" -> (opt => airplan5Example(getWorkingDir(opt))),
    )
  }

  def getBenchmarks(target: String, cliOption: CliOption): Iterator[(String, FuzzingTaskProvider)] = {
    target match {
      case "all" => benchmarks.toIterator.map {
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
    val parser = cliOptParser()

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
                Runner.runExample(bench, cliOption.ioId, cliOption.seeds, config, useGUI = !cliOption.disableGui)
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

      opt[Int]('p', "population-size").action((x, c) =>
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

      help("help").text("Prints this usage text")
      version("version").text("Prints the version info")

      override def showUsageOnError = true

      arg[String]("<target>").required().action((x, c) =>
        c.copy(target = x)).text("Benchmark target to run.")

      note("\nBenchmark target list:\n" + benchmarks.keys.map(s => "  " + s).mkString("\n") + "\n")
    }
  }


}
