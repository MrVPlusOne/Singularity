package cli

import benchmarks.SlowfuzzExamples
import patsyn.Runner.RunnerConfig
import patsyn._
import scopt.OptionParser

object SlowfuzzExperiment {

  case class CliOption(name: String,
                       size: Int = 256,
                       ioId: Int = 0,
                       seed: Int = 0,
                       fuzzingTime: Int = 3600,
                       disableGui: Boolean = false,
                       manualParamTuning: Boolean = false)

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

  private val gpParamMap = Map[String, GPConfig](
    "appleqsort" -> GPConfig(populationSize = 700, totalSizeTolerance = 70, crossoverP = 0.5, mutateP = 0.7, copyP = 0.2),
    "bsdqsort" -> GPConfig(populationSize = 700, totalSizeTolerance = 70, crossoverP = 0.5, mutateP = 0.7, copyP = 0.2),
    "pcre_regex2" -> GPConfig(populationSize = 1000, totalSizeTolerance = 120, crossoverP = 0.5, mutateP = 0.7, copyP = 0.2),
    "pcre_regex13" -> GPConfig(totalSizeTolerance = 90, crossoverP = 0.5, mutateP = 0.4, copyP = 0.25),
    "phphash" -> GPConfig(populationSize = 650, totalSizeTolerance = 70, singleSizeTolerance = 25, crossoverP = 0.5, mutateP = 0.7, copyP = 0.2)
  )
  private val gpConfigMap = Map[String, GPEnvironment](
    "appleqsort" -> FuzzingTaskProvider.sortingEnv,
    "bsdqsort" -> FuzzingTaskProvider.sortingEnv,
    "pcre_regex2" -> FuzzingTaskProvider.asciiRegexEnv,
    "pcre_regex13" -> FuzzingTaskProvider.asciiRegexEnv,
    "phphash" -> FuzzingTaskProvider.hashEnv
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
    ExecutionConfig(
      evalSizePolicy = FixedEvalSize(cliOption.size),
      maxNonIncreaseGen = None,
      maxFuzzingTimeSec = Some(cliOption.fuzzingTime),
      timeLimitInMillis = 900000  // Be lenient about this as strange timeouts occurs frequently
    )
  }

  def getGPConfig(cliOption: CliOption): GPConfig = {
    gpParamMap.getOrElse(cliOption.name, {
      println(s"Cannot find overriding gp params for benchmark: ${cliOption.name}")
      sys.exit(-1)
    })
  }

  def getGPEnv(cliOption: CliOption): GPEnvironment = {
    gpConfigMap.getOrElse(cliOption.name, {
      println(s"Cannot find overriding gp env for benchmark: ${cliOption.name}")
      sys.exit(-1)
    })
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
    if (cliOption.manualParamTuning) {
      val gpConfig = getGPConfig(cliOption)
      val gpEnv = getGPEnv(cliOption)
      val config = RunConfig(runnerConfig, gpConfig, execConfig)
      Runner.run(problem, gpEnv, config)
    } else {
      Supernova.fuzzProblem(problem, runnerConfig, execConfig, new scala.util.Random(cliOption.seed))
    }
    println(s"*** Task $cliOption finished ***")
  }

  def main(args: Array[String]): Unit = {
    val parser = optParser()
    parser.parse(args, CliOption(name = "unknown")).foreach(runExperiment)
  }

  def optParser(): OptionParser[CliOption] = {
    new scopt.OptionParser[CliOption]("slowfuzz_exp_driver") {
      head("Slowfuzz Benchmark Driver", "0.1")

      opt[Unit]('n', "no-gui").action((_, c) =>
        c.copy(disableGui = true)).text("Disable the GUI panel.")

      opt[Unit]('m', "manual").action((_, c) =>
        c.copy(manualParamTuning = true)).text("Try to override supernova with manually tuned GP params")

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
