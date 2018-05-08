package patsyn

object IOExample {

  import java.io.File

  val TIME_REGEX = """.*\[time=(\d+)\].*""".r

  val SEED_REGEX = """.*seed=(\d+).*""".r

  def processFile(name: String, filePath: String): (Int, Double) = {
    val inputs = FileInteraction.readObjectFromFile[Vector[EValue]](filePath)
    val timestamp = new File(filePath).getName match {
      case TIME_REGEX(digits) =>
        digits.mkString("").toInt
    }
    val perf = FuzzingTaskProvider.phpHashPerformanceExample.runAsProbConfig(name){ config =>
      config.resourceUsage(inputs)
    }
    (timestamp, perf)
  }

  def processDir(name: String, dirPath: String): (Int, IS[(Int, Double)]) = {
    val dirFile = new File(dirPath)
    assert(dirFile != null)
    val seed = dirFile.getName match {
      case SEED_REGEX(digits) =>
        digits.mkString("").toInt
    }
    val pairs = for (f <- dirFile.list() if f.startsWith("bestInput") && f.endsWith(".serialized")) yield {
      processFile(name, s"$dirPath/$f")
    }
    (seed, pairs.sortBy{
      case (s, _) => s
    })
  }

  def writeCsv(filePath: String, data: IS[(Int, Double)]) = {
    val content = data.map {
      case (t, p) => s"$t, $p"
    }.mkString("\n")
    FileInteraction.writeToFile(filePath)(content)
  }

  def getPerf(t: Int)(series: IS[(Int, Double)]): Double = {
    val idx = series.indexWhere {
      case (tpoint, _) =>
        t < tpoint
    }
    if (idx == -1)
      series.last._2
    else if (idx == 0)
      0.0
    else
      series(idx - 1)._2
  }

  def processTopDir(inName: String, outName: String) = {
    val funcs = for (d <- new File(inName).list()) yield {
      val (_, data) = processDir("hashperf", s"$inName/$d")
      data
    }

    val startTime = 0
    val endTime = 3600
    val tperfs = for (ts <- startTime until endTime) yield {
      val tms = ts * 1000
      val perfs = funcs.map(getPerf(tms))
      println(perfs.toIndexedSeq)
      val avg_perf = perfs.sum / perfs.length
      (tms, avg_perf)
    }

    writeCsv(outName, tperfs)
  }

  def main(args: Array[String]): Unit = {
    val inDir1 = "/home/grieve/scratch/results1/hashperf"
    val outFile1 = "/home/grieve/scratch/results1/out.csv"
    val inDir2 = "/home/grieve/scratch/results2/hashperf"
    val outFile2 = "/home/grieve/scratch/results2/out.csv"

    processTopDir(inDir1, outFile1)
    processTopDir(inDir2, outFile2)
  }
}
