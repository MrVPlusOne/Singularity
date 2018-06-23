package singularity.measure


import java.io.ByteArrayInputStream
import singularity.measure.TimeMeasurement.DoubleAsMillis
import singularity.FileInteraction

object TimeMeasurement{
  type DoubleAsMillis = Double
}

class TimeMeasurement[Input](computation: Input => sys.process.Process, tearDown: Input => Unit) {
  import scala.concurrent._
  import ExecutionContext.Implicits.global


  def measure(input: Input, timeout: DoubleAsMillis): DoubleAsMillis = {
    val timeoutMillis = timeout.toLong
    val p = computation(input)
    val f = Future(blocking(p.exitValue())) // wrap in Future
    try {
      val (nano, _) = TimeTools.measureTime {
        Await.result(f, duration.Duration(timeoutMillis, "ms"))
      }
      nano.toDouble / 1e6
    } catch {
      case _: TimeoutException =>
        p.destroy()
        timeout
    } finally {
      tearDown(input)
    }
  }
}


object TimeMeasureExamples {
  import java.io.File

  def phpHashExample = {
    def tempFileName = {
      val threadId = Thread.currentThread().getId
      s"tempGen/phpHash$threadId.json"
    }
    new TimeMeasurement[Seq[String]](input => {

      FileInteraction.runWithAFileLogger(tempFileName, printToConsole = false) { logger =>
        val table = input.map(s => "\"" + s + "\": null").mkString("{", ",", "}")
        logger.println(table)
      }
      //    val table = input.map(s => "\\\"" + s + "\\\": null").mkString("{",",","}")
      import sys.process._

      Seq("php", "-r", s"""json_decode(file_get_contents(\"$tempFileName\"), true);""").run()
      //      Seq("php", "-r", s"""json_decode(file_get_contents(\"tempGen/1048576.json\"), true);""").run()
    }, tearDown = _ => {
      new File(tempFileName).delete()
    })
  }

  def phpHashExampleNoFile = {
    new TimeMeasurement[Seq[String]](input => {
      val table = input.map(s => "\"" + s + "\": null").mkString("{", ",", "}")
      import sys.process._

//      Seq("echo", table).run()
    val is = new ByteArrayInputStream(table.getBytes("UTF-8"))
      (Seq("php", "-r", "json_decode(file_get_contents(\"php://stdin\"), true);") #< is).run()
    }, tearDown = _ => {

    })
  }

//  def main(args: Array[String]): Unit = {
//    println{
//      phpHashExample.measure(Seq("a","b","c","d","e","f","g","h"), 1000)
//    }
//  }
}