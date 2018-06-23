package singularity.benchmarks

import ammonite.ops._
import singularity.measure.TimeTools
import singularity.EvolutionRepresentation.IndividualEvaluation
import singularity.Runner.RunnerConfig
import singularity._

import scala.util.Random

object SlowFuzzRerun {

  def runTask(args: Array[String]): Unit ={
    val evalSize = 1024
    val threadNum = 4
    val hoursAllowed = 10
    val maxIteration = 100
    val logPath: Path = pwd / "analyzed" / "slowFuzzRerun.txt"
    var currentBestPerformance = -1000.0


    FileInteraction.mkDirsAlongPath("analyzed")
    val execConfigTemplate: ExecutionConfig = ExecutionConfig(evalSizePolicy = FixedEvalSize(evalSize), timeLimitInMillis = 10 * 60 * 1000)

    SimpleMath.parallelMap(threadNum)(0 until threadNum){
      i =>
        val baseSeed = i * 10000
        var timeLeft = (3600 * hoursAllowed).toLong
        var numIters = 0

        while (timeLeft > 0L && numIters < maxIteration) {
          val seed = baseSeed + numIters
          val problem: ProblemConfig = SlowfuzzExamples.appleQsortIntExample(evalSize)(FileInteraction.getWorkingDir(i))
          def reportResult(multiStateInd: MultiStateInd, individualEvaluation: IndividualEvaluation): Unit = this.synchronized {
            if(individualEvaluation.performance > currentBestPerformance){
              currentBestPerformance = individualEvaluation.performance
              FileInteraction.writeToFile(logPath.toString(), append = true){
                s"new best performance[seed=$seed]: $currentBestPerformance\n"
              }
            }
          }

          val runnerConfig = RunnerConfig(randomSeed = seed, ioId = seed, useGUI = true, callExitAfterFinish = false,
            reportResult = reportResult
          )
          try {
            val (timeUsed, _) = TimeTools.measureTime {
              val execConfig = execConfigTemplate.copy(maxFuzzingTimeSec = Some(timeLeft))
              Supernova.standardSupernova.fuzzProblem(problem, runnerConfig, execConfig,
                new Random(seed))
            }
            timeLeft -= (timeUsed / 1000000000)
          } catch {
            case tE: Runner.MaxFuzzingTimeReachedException =>
              println(s"Benchmark $i finished, time limit for this one: ${tE.timeLimitSec}")
              timeLeft = -1
          }
          numIters += 1
        }
    }
  }

  def main(args: Array[String]): Unit = {
    runTask(args)
  }

}
