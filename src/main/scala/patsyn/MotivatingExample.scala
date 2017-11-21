package patsyn

import patsyn.Runner.{RunConfig, runExample}

object MotivatingExample {
  def main(args: Array[String]): Unit = {
    val ioId = 0
    val workingDir = s"workingDir$ioId"
    FileInteraction.mkDirsAlongPath(workingDir)

    runExample(FuzzingTaskProvider.quickSortExample, ioId, Seq(ioId), RunConfig.default.copy(populationSize = 300))
  }
}
