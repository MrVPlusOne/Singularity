package singularity.cli

import singularity.benchmarks.SlowfuzzExamples
import singularity.Runner.RunnerConfig
import singularity._
import scopt.OptionParser

object SlowfuzzExperiment {

  case class CliOption(name: String,
                       size: Int = 256,
                       ioId: Int = 0,
                       seed: Int = 0,
                       fuzzingTime: Int = 3600,
                       disableGui: Boolean = false)

  private val problemMap = Map[String, Int => String => ProblemConfig](
    "isort" -> SlowfuzzExamples.insertionSortIntExample,
    "qsort" -> SlowfuzzExamples.slowfuzzQsortIntExample,
    "appleqsort" -> SlowfuzzExamples.appleQsortIntExample,
    "bsdqsort" -> SlowfuzzExamples.bsdQsortIntExample,
    "pgqsort" -> SlowfuzzExamples.pgQsortIntExample,
    "gnuqsort" -> SlowfuzzExamples.gnuQsortIntExample,
    "phphash" -> SlowfuzzExamples.phpHashExample
  ) ++ (0 until 20).map(i =>
    s"pcre_regex$i" -> SlowfuzzExamples.pcreExample(i) _
  )

  def getRunnerConfig(cliOption: CliOption): RunnerConfig = {
    RunnerConfig(
      ioId = cliOption.ioId,
      randomSeed = cliOption.seed,
      useGUI = !cliOption.disableGui,
      keepBestIndividuals = true // Keep all the individuals in case we need them
    )
  }

  def getExecConfig(cliOption: CliOption): ExecutionConfig = {
    val problemSize = if (!cliOption.name.startsWith("pcre_regex")) cliOption.size / 4 else cliOption.size // Sorting examples work on int32 (4x byte)
    ExecutionConfig(
      evalSizePolicy = FixedEvalSize(problemSize),
      maxNonIncreaseGen = None,
      maxFuzzingTimeSec = Some(cliOption.fuzzingTime),
      timeLimitInMillis = 900000  // Be lenient about this as strange timeouts occurs frequently
    )
  }

  def getProblem(cliOption: CliOption): ProblemConfig = {
    problemMap.getOrElse(cliOption.name, {
      println(s"Cannot find benchmark: ${cliOption.name}")
      sys.exit(-1)
    })(cliOption.size)(FileInteraction.getWorkingDir(cliOption.ioId))
  }

  def runExperiment(cliOption: CliOption) = {
    val runnerConfig = getRunnerConfig(cliOption)
    val execConfig = getExecConfig(cliOption)
    val problem = getProblem(cliOption)
    println(s"*** Task $cliOption started ***")
    Supernova.standardSupernova.fuzzProblem(problem, runnerConfig, execConfig, new scala.util.Random(cliOption.seed))
    println(s"*** Task $cliOption finished ***")
  }

//  def main(args: Array[String]): Unit = {
//    val parser = optParser()
//    parser.parse(args, CliOption(name = "unknown")).foreach(runExperiment)
//  }

  def optParser(): OptionParser[CliOption] = {
    new scopt.OptionParser[CliOption]("slowfuzz_exp_driver") {
      head("Slowfuzz Benchmark Driver", "0.1")

      opt[Unit]('n', "no-gui").action((_, c) =>
        c.copy(disableGui = true)).text("Disable the GUI panel.")

      opt[Int]('i', "ioId").action((id, c) =>
        c.copy(ioId = id)).text("The id used to perform IO actions. If you have n processes running at the same time, just set their idIo to 0 through n.")

      opt[Int]('s', "seed").action((s, c) =>
        c.copy(seed = s)).text("The random seed to use. Default to 0.")

      opt[Int]('t', "timeout").action((t, c) =>
        c.copy(fuzzingTime = t)).text("Fuzzing timeout (in sec). Default to 3600")

      help("help").text("Prints this usage text")
      version("version").text("Prints the version info")

      arg[String]("<target>").required().action((x, c) =>
        c.copy(name = x)).text("Benchmark target to run.")
      arg[Int]("<size>").required().action((x, c) =>
        c.copy(size = x)).text("Input size in bytes.")

      override def showUsageOnError = true

      note("\nBenchmark target list:\n" + problemMap.keys.toSeq.sorted.map(s => "  " + s).mkString("\n") + "\n")
    }
  }
}
