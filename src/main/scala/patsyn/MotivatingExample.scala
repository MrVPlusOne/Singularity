package patsyn

import patsyn.Runner.{RunConfig, runExample}

object MotivatingExample {
  def main(args: Array[String]): Unit = {
    val ioId = 0
    val workingDir = s"workingDir$ioId"
    FileInteraction.mkDirsAlongPath(workingDir)

//    runExample(FuzzingTaskProvider.gabFeed2Example(ioId ,workingDir), Seq(ioId), useGUI = true)
//    runExample(FuzzingTaskProvider.textCrunchrExample(workingDir), Seq(ioId), useGUI = true)
    runExample(FuzzingTaskProvider.quickSortExample, Seq(ioId), RunConfig.default.copy(populationSize = 300))
//    runExample(FuzzingTaskProvider.regexExample("^abc*!"))
  }
}
