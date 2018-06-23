package singularity.benchmarks

import ammonite.ops._
import singularity.benchmarks.ExampleAlgorithms.parseCost
import singularity.{FileInteraction, FuzzingTaskProvider, ProblemConfig}

import scala.util.matching.Regex

object SlowFuzzResultAnalyze {
  case class SlowFuzzResult(name: String, size: Int, trial: Int, path: Path)

  val slowFuzzResultRegex: Regex = s"""^(.+)_size(.+)_trial_(.+)""".r

  def analyze(resultsPath: Path, logPath: Path): Unit = {
    val results = (ls ! resultsPath).map(f => (f, f.name)).collect {
      case (f, slowFuzzResultRegex(name, sizeS, trialS)) =>
        SlowFuzzResult(name, sizeS.toInt, trialS.toInt, f)
    }.groupBy(r => (r.name, r.size)).toIndexedSeq.sortBy(_._1)

    println(results)

    val mapToScores = results.map { case ((name, size), trials) =>
      println(s"process $name at size $size")
      val runner = getProblemRunner(name)
      val maxScore = (for(
        r <- trials;
        inputPath <- ls(r.path / "output")
      ) yield {
        val score = runner(inputPath.toString())
        println(score)
        score
      }).max

      FileInteraction.writeToFile(logPath.toString(), append = true){
        s"$name, $size, $maxScore\n"
      }
      (name, size) -> maxScore
    }

    println("Eval finished!")
    mapToScores.foreach(println)
  }


  val isRegexExample: Regex = s"""pcre_regex(.+)""".r
  def getProblemRunner(name: String): String => Double = {
    val (executable, extraArgs) = name match {
      case isRegexExample(iS) =>
        "pcre_str" -> Seq(iS)
      case _ if name.endsWith("sort") =>
        (name + "_int") -> Seq()
      case _ =>
        name -> Seq()
    }

    val execPath = nativeBinaryPath(executable)

    (file: String) => {
      import sys.process._
      try {
        val results = (Seq(execPath, file) ++ extraArgs).lineStream
        val cost = parseCost(results.last)
        cost.toDouble
      } catch {
        case _: RuntimeException =>
          // Program crashes are not interesting to us
          0
      }
    }
  }

  def nativeBinaryPath(name: String): String = {
    def ensureExecutable(filePath: String): Unit = {
      val file = new java.io.File(filePath)
      if (!file.exists)
        throw new RuntimeException(s"native binary file \'$filePath\' does not exist")
      if (!file.isFile)
        throw new RuntimeException(s"\'$filePath\' is not a file")
      if (!file.canExecute)
        throw new RuntimeException(s"\'$filePath\' is not executable")
    }

    val nativeBinaryPath: String = {
      val arch = System.getProperty("os.arch") match {
        case "x86_64" | "amd64" => "amd64"
        case _@a => throw new RuntimeException(s"We currently do not prepare native binaries for architecture \'$a\'")
      }
      val os = System.getProperty("os.name") match {
        case "Mac OS X" => "macos"
        case "Linux" => "linux"
        case _@o => throw new RuntimeException(s"We currently do not prepare native binaries for OS \'$o\'")
      }
      val binPath = s"benchmarks/native/${arch}_${os}/$name"
      ensureExecutable(binPath)
      binPath
    }
    nativeBinaryPath
  }

  def main(args: Array[String]): Unit = {
    analyze(Path.home / "Downloads" / "slowFuzzResults", pwd / "analyzed" / "slowFuzzResults.txt")
  }
}
