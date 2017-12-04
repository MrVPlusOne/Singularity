package patsyn

import patsyn.Runner.runExample

object MotivatingExample {
  def main(args: Array[String]): Unit = {
    val ioId = 0
    val workingDir = s"workingDir$ioId"
    FileInteraction.mkDirsAlongPath(workingDir)

    val defaultConfig = RunConfig.default
    runExample("quickSort", FuzzingTaskProvider.quickSortExample,
      defaultConfig.copy(
        gpConfig = defaultConfig.gpConfig.copy(populationSize = 300)
      )
    )
  }
}
