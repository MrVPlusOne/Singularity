package patsyn

object IOExample {
  def main(args: Array[String]): Unit = {
    val inputs = FileInteraction.readObjectFromFile[Vector[EValue]]("/Users/weijiayi/Downloads/results 6/hashperf/php[performance=16756.0][ioId=11,seed=1011](18-05-07-19:08:35)/bestInput[time=3584727].serialized")


    val usage = FuzzingTaskProvider.phpHashPerformanceExample.runAsProbConfig("bu zhong yao"){ config =>
      config.resourceUsage(inputs)
    }
    println(usage)
  }
}
