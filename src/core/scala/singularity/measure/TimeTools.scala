package singularity.measure

import java.text.SimpleDateFormat


/**
  * Time measurement utilities
  */
object TimeTools {
  type Nanosecond = Long

  def nanoToMillisString(nanosecond: Nanosecond): String = {
    val millis = (nanosecond /1e9).toInt
    if(millis>0){
      val ms = nanosecond/1e6 - millis * 1000
      "%d,%06.2fms".format(millis.toInt, ms)
    } else
      "%.2fms".format(nanosecond /1e6)
  }

  def nanoToSecondString(nanosecond: Nanosecond): String = {
    "%.3fs".format(nanosecond/1e9)
  }

  def printTimeUsed[A](taskName: String, shouldPrint: Boolean = true)(task: => A): A = {
    val (nano, result) = measureTime(task)
    if(shouldPrint)
      println(s"*** [$taskName] time used: ${nanoToMillisString(nano)} ***")
    result
  }

  def measureTime[A](task: => A): (Nanosecond, A) = {
    val t1 = System.nanoTime()
    val result = task
    val time = System.nanoTime() - t1
    (time, result)
  }

  /** Since this method uses Thread.sleep, it may not be accurate for methods with very short running time */
  def scaleUpRunningTime[A](factor: Int)(task: => A): A = {
    require(factor >= 1)

    if(factor == 1) return task

    val (nano, result) = measureTime(task)
    val extraNano = (factor - 1) * nano
    val millis = extraNano / 1000000
    val nanos = extraNano - millis * 1000000
    Thread.sleep(millis, nanos.toInt)
    result
  }

  @inline
  def runOnce[A](f: => A): A = {
    f
  }

  @inline
  def run5Times[A](f: => A): A = {
    f;f;f;f;f
  }

  def runWithTimeout[T](timeoutMs: Millisecond)(f: => T) : Option[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    import scala.concurrent.duration._

    try {
      Some(Await.result(Future(f), timeoutMs.milliseconds))
    } catch {
      case e: TimeoutException => None
    }
  }

  type Millisecond = Long
  type Timer = () => Millisecond
  def runWithATimer[T](f: Timer => T): T = {
    val startTime = System.currentTimeMillis()
    val timer = () => {
      System.currentTimeMillis() - startTime
    }
    f(timer)
  }

  val numericalDateTimeFormat = new SimpleDateFormat("yy-MM-dd-HH:mm:ss")

  def numericalDateTime(): String = {
    import java.util.Calendar

    val date = Calendar.getInstance().getTime
    numericalDateTimeFormat.format(date)
  }

}
