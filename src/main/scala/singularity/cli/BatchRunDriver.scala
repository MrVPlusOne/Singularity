package singularity.cli

import singularity.Runner.RunnerConfig
import singularity.{ExecutionConfig, SimpleMath}
import scopt.OptionParser

case class BatchRunOption(benchNames: Seq[String] = Seq(),
                          jobNumber: Int = 1,
                          benchTrials: Int = 1,
                          execConfig: ExecutionConfig = ExecutionConfig().copy(maxNonIncreaseGen = None),
                          sizeOfInterestOverride: Option[Int] = None,  // merely to get around with the fact that ExecutionConfig.sizeOfInterest is not an optional
                          keepBestIndividuals: Boolean = false,
                          startIoId: Int = 0,
                          startSeed: Int = 0)

case class SingleRunOption(benchName: String, executionConfig: ExecutionConfig, runnerConfig: RunnerConfig, sizeOfInterestOverride: Option[Int])

object BatchRunDriver {

  private def runCmd(cmd: String) = {
    import sys.process._
    cmd.split("\\s+").toSeq.!
  }

  def runSingle(opt: SingleRunOption): Unit = {


    import java.io.File
    import java.nio.file.Paths
    val javaPath = Paths.get(System.getProperty("java.home"), "bin", "java").toFile.getAbsolutePath
    val jarPath = new File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath).getAbsolutePath

    val timeoutOpt = s"--time-limit ${opt.executionConfig.timeLimitInMillis}"
    val keepOpt = if (opt.runnerConfig.keepBestIndividuals) "-k" else ""
    val maxNonIncreaseOpt = opt.executionConfig.maxNonIncreaseGen.map(gen => s"--max-nonincrease-gen $gen").getOrElse("")
    val maxFuzzingTimeOpt = opt.executionConfig.maxFuzzingTimeSec.map(t => s"--max-fuzzing-time $t").getOrElse("")
    val taskSizeOpt = opt.sizeOfInterestOverride.map(size => s"--task-size $size").getOrElse("")
    val cmdOpts = s"-i ${opt.runnerConfig.ioId} -s ${opt.runnerConfig.randomSeed} -n $timeoutOpt $keepOpt $maxNonIncreaseOpt $maxFuzzingTimeOpt $taskSizeOpt ${opt.benchName}"
    val cmd = s"$javaPath -cp $jarPath cli.BenchmarkDriver $cmdOpts"

    println(s"[JOB STARTED] $opt")
    println(cmd)
    runCmd(cmd)
    println(s"[JOB FINISHED] $opt")
  }

  def runBatch(opt: BatchRunOption): Unit = {
    val benchNameSeeds =
      for { bench <- opt.benchNames; seed <- opt.startSeed until opt.startSeed + opt.benchTrials} yield (bench, seed)
    val singleConfigs = benchNameSeeds.zipWithIndex.map {
      case ((benchName: String, seed: Int), ioId: Int) =>
        val runnerConfig = RunnerConfig().copy(ioId = ioId, randomSeed = seed, useGUI = false, keepBestIndividuals = opt.keepBestIndividuals)
        SingleRunOption(benchName, opt.execConfig, runnerConfig, sizeOfInterestOverride = opt.sizeOfInterestOverride)
    }

    SimpleMath.parallelMapOrdered(singleConfigs, opt.jobNumber)(runSingle)
  }

  def main(args: Array[String]): Unit = {
    val parser = batchRunnerOptParser()
    parser.parse(args, BatchRunOption()).foreach(runBatch)
  }

  def batchRunnerOptParser(): OptionParser[BatchRunOption] = {
    new scopt.OptionParser[BatchRunOption]("patsyn_batch_runner") {
      head("patsyn Batch Experiment Runner", "0.1")

      opt[Int]('j', "job-number").action((x, c) =>
        c.copy(jobNumber = x)).text("Maximum number of jobs that will be scheduled simultaneously. Default to 1.")

      opt[Int]('t', "trials").action((x, c) =>
        c.copy(benchTrials = x)).text("Try to run each benchmark target with this number of different random seeds. " +
        "Default to 1.")

      opt[Unit]('k', "keep-best-individuals").action((_, c) =>
        c.copy(keepBestIndividuals = true)).text("Each time the tool finds a better individual, preserve it in a " +
        "separated file instead of overwriting the file that stores the previous best one. Off by default.")

      opt[Int]("time-limit").action((x, c) =>
        c.copy(execConfig = c.execConfig.copy(timeLimitInMillis = x))).text("Time limit for each black-box execution (in milliseconds). Default to 120000.")

      opt[Int]("job-timeout").action((x, c) =>
        c.copy(execConfig = c.execConfig.copy(maxFuzzingTimeSec = Some(x)))).text("Timeout for each job (in seconds). Default to infinity. ")

      opt[Int]("max-nonincrease-gen").action((x, c) =>
        c.copy(execConfig = c.execConfig.copy(maxNonIncreaseGen = Some(x)))).text("Stop the program if the best fitness has not increased for this number of generations. Dose not specify this value means it should never stop.")

      opt[Int]("task-size").hidden().action((x, c) =>
        c.copy(sizeOfInterestOverride = Some(x))).text("Manually override the problem size for the fuzzing task. Only specify this when you know what you are doing.")

      opt[Int]("base-ioid").action((id, c) =>
        c.copy(startIoId = id)
      ).text("The runner will try to assign ioid starting from this number. Default to 0")

      opt[Int]("base-seed").action((s, c) =>
        c.copy(startSeed = s)).text("The runner will try to assign random seeds starting from this number. Default to 0")

      help("help").text("Prints this usage text")
      version("version").text("Prints the version info")

      override def showUsageOnError = true

      arg[String]("<target>...").unbounded().required().action( (x, c) =>
        c.copy(benchNames = c.benchNames :+ x) ).text("Benchmark targets to run")
    }
  }
}
