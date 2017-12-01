package cli

import patsyn.SimpleMath
import scopt.OptionParser

case class BatchRunOption(benchNames: Seq[String] = Seq(),
                          jobNumber: Int = 1,
                          benchTrials: Int = 1,
                          startIoId: Int = 0,
                          startSeed: Int = 0)

case class SingleRunOption(benchName: String, ioId: Int, seed: Int)

object BatchRunDriver {

  def runSingle(opt: SingleRunOption): Unit = {
    println(s"[JOB STARTED] $opt")

    import java.io.File
    import java.nio.file.Paths
    val javaPath = Paths.get(System.getProperty("java.home"), "bin", "java").toFile.getAbsolutePath
    val jarPath = new File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath).getAbsolutePath
    // TODO: Make timeout configurable
    val cmdOpts = s"-i ${opt.ioId} -s ${opt.seed} -n --time-limit 120000 ${opt.benchName}"
    val cmd = s"$javaPath -jar $jarPath $cmdOpts"
    println(cmd)

    import sys.process._
    cmd.split("\\s+").toSeq.!

    println(s"[JOB FINISHED] $opt")
  }

  def runBatch(opt: BatchRunOption): Unit = {
    val benchNameSeeds =
      for { bench <- opt.benchNames; seed <- opt.startSeed until opt.startSeed + opt.benchTrials} yield (bench, seed)
    val singleConfigs = benchNameSeeds.zipWithIndex.map {
      case ((benchName: String, seed: Int), ioId: Int) =>
        SingleRunOption(benchName, ioId, seed)
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

      opt[Int]("base-ioid").action((id, c) =>
        c.copy(startIoId = id)
      ).text("The runner will try to assign ioid starting from this number. Default to 0")

      opt[Int]("base-seed").action((s, c) =>
        c.copy(startSeed = s)).text("The runner will try to assign random seeds starting from this number. Default to 0")

      help("help").text("Prints this usage text")
      version("version").text("Prints the version info")

      override def showUsageOnError = true

      arg[String]("<target>...").unbounded().optional().action( (x, c) =>
        c.copy(benchNames = c.benchNames :+ x) ).text("Benchmark targets to run")
    }
  }
}
