package patsyn

import patsyn.Runner.runExample

object MotivatingExample {
  def main(args: Array[String]): Unit = {
    val ioId = 0
    val workingDir = s"workingDir$ioId"
    FileInteraction.mkDirsAlongPath(workingDir)

    val example = FuzzingTaskProvider.quickSortMiddlePivotExample
//    val defaultConfig = RunConfig.default
//    runExample("quickSort", example,
//      defaultConfig.copy(
//        gpConfig = defaultConfig.gpConfig.copy(populationSize = 300)
//      )
//    )
    import StandardSystem._

    def toVectValue(xs: IS[Int]) = VectValue(xs.map(IntValue.apply).toVector)

    val pattern1 = (0 until 100).foldLeft(IS[Int]()){(acc, i) => acc :+ i }
    val pattern2 = (0 until 100).foldLeft(IS[Int]()){(acc, i) => i +: acc }
    val pattern3 = (0 until 100).foldLeft(IS[Int]()){(acc, i) => (2*i+1) +: acc :+ (2*i) }
    example.run{
       task =>
         println{task.resourceUsage(IS(toVectValue(pattern1)))}
         println{task.resourceUsage(IS(toVectValue(pattern2)))}
         println{task.resourceUsage(IS(toVectValue(pattern3)))}
    }
  }
}
