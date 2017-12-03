package cli

import patsyn.SimpleMath
import scopt.OptionParser

case class BatchRunOption(benchNames: Seq[String] = Seq(),
                          jobNumber: Int = 1,
                          benchTrials: Int = 1,
                          keepBestIndividuals: Boolean = false,
                          timeLimitInMillis: Int = 120000,
                          jobTimeout: Option[Int] = None,
                          startIoId: Int = 0,
                          startSeed: Int = 0)

case class SingleRunOption(benchName: String, ioId: Int, seed: Int, evalTimeLimit: Int, jobTimeout: Option[Int], keepBestIndividuals: Boolean)

object BatchRunDriver {

  private def runCmdWithTimeout(cmd: String, sec: Int) = {
    import sys.process._
    import scala.concurrent._
    import ExecutionContext.Implicits.global
    val proc = cmd.run()
    val f = Future(blocking(proc.exitValue()))
    try
      Await.result(f, duration.Duration(sec, "sec"))
    catch {
      case _: TimeoutException =>
        println("TIMEOUT!")
        proc.destroy()
        proc.exitValue()
    }
  }

  private def runCmdWithoutTimeout(cmd: String) = {
    import sys.process._
    cmd.split("\\s+").toSeq.!
  }

  def runSingle(opt: SingleRunOption): Unit = {
    println(s"[JOB STARTED] $opt")

    import java.io.File
    import java.nio.file.Paths
    val javaPath = Paths.get(System.getProperty("java.home"), "bin", "java").toFile.getAbsolutePath
    val jarPath = new File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath).getAbsolutePath

    val timeoutOpt = s"--time-limit ${opt.evalTimeLimit}"
    val keepOpt = if (opt.keepBestIndividuals) "-k" else ""
    val maxNonIncreaseOpt = if (opt.jobTimeout.isDefined) "--max-nonincrease-gen -1" else ""
    val cmdOpts = s"-i ${opt.ioId} -s ${opt.seed} -n $timeoutOpt $keepOpt $maxNonIncreaseOpt ${opt.benchName}"
    val cmd = s"$javaPath -cp $jarPath cli.BenchmarkDriver $cmdOpts"
    println(cmd)

    opt.jobTimeout match {
      case Some(i) =>
        println(s"[SET JOB TIMEOUT] $i sec")
        runCmdWithTimeout(cmd, i)
      case None =>
        runCmdWithoutTimeout(cmd)
    }

    println(s"[JOB FINISHED] $opt")
  }

  def runBatch(opt: BatchRunOption): Unit = {
    val benchNameSeeds =
      for { bench <- opt.benchNames; seed <- opt.startSeed until opt.startSeed + opt.benchTrials} yield (bench, seed)
    val singleConfigs = benchNameSeeds.zipWithIndex.map {
      case ((benchName: String, seed: Int), ioId: Int) =>
        SingleRunOption(benchName, ioId, seed, opt.timeLimitInMillis, opt.jobTimeout, opt.keepBestIndividuals)
    }

    SimpleMath.parallelMap(singleConfigs, runSingle, opt.jobNumber)
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
        c.copy(timeLimitInMillis = x)).text("Time limit for each black-box execution (in milliseconds). Default to 120000.")

      opt[Int]("job-timeout").action((x, c) =>
        c.copy(jobTimeout = Some(x))).text("Timeout for each job (in seconds). Default to infinity. ")

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
