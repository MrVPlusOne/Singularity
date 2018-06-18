package patsyn

import scala.util.Random

object RunRandomSearch {
  def main(args: Array[String]): Unit = {
    val ioId = 1
    val rand = new Random(ioId)

    val taskProvider = FuzzingTaskProvider.phpHashPerformanceExample

    val nova = Supernova.standardSupernova
    taskProvider.runAsProbConfig("Random-PhpHashPerformance"){ config =>
      val (env, gpConfig) = nova.genGPParameters(config.outputTypes, rand)


      Runner.run(config, env)

      ???
    }
  }
}
