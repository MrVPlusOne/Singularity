package patsyn

import scala.util.Random

object SimpleMath {
  def wrapInRange(i: Int, range: Int): Int = {
    val m = i % range
    if (m < 0) m + range else m
  }

  def square(x: Double): Double = x * x

  def gaussianForthOrder(halfPoint: Double)(x: Double): Double = {
    math.pow(2,-math.pow(x/halfPoint,4))
  }

  def randomSelect[A](random: Random)(xs: IndexedSeq[A]): A = {
    val i = random.nextInt(xs.length)
    xs(i)
  }

  def randomGuess(random: Random)(pTrue: Double): Boolean = {
    random.nextDouble() < pTrue
  }

  def natToList(n: Int, base: Int): List[Int] = {
    require(n>=0)
    require(base > 0)
    def rec(n: Int): List[Int] = {
      if(n==0) List()
      else{
        val residual = n % base
        residual :: rec(n/base)
      }
    }
    val r = rec(n).reverse
    if(r.isEmpty) List(0) else r
  }

  def aggressiveSigmoid(aggressiveness: Double) = (x: Double) => {
    val a = math.pow(50, 2*(0.5 - aggressiveness))
    math.pow(x,a)
  }

  def aggressiveInterpolate(aggressiveness: Double, from: Double, to: Double)(x: Double) = {
    linearInterpolate(from, to)(aggressiveSigmoid(aggressiveness)(x))
  }

  def linearInterpolate(from: Double, to: Double)(x: Double): Double = {
    (to - from) * x + from
  }

  def expInterpolate(from: Double, to: Double)(x: Double): Double = {
    val ratio = to/from
    from*math.pow(ratio, x)
  }

  def sigmoid(x: Double) = 1.0/(1+math.exp(-x))

  def parallelMap[A,B](seq: Seq[A], threadNum: Int)(f: A => B): Seq[B] = {
    require(threadNum>0)
    import scala.collection.parallel
    import parallel._

    val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadNum))

    if(threadNum>1) {
      val p = seq.par
      p.tasksupport = taskSupport
      p.map(f).toIndexedSeq
    }else{
      seq.map(f).toIndexedSeq
    }
  }

  /**
    * Perform a map on a [[Seq]] in parallel. The order of starting each task is preserved as the [[Seq]].  */
  def parallelMapOrdered[A,B](seq: Seq[A], threadNum: Int, timeoutSec: Int = 20474834)(f: A => B): Seq[B] = {
    require(threadNum>0)

    if(threadNum>1) {
      import akka.actor._
      import scala.concurrent.duration._

      object StartTask
      case class EvalTask(content: A)
      case class EvalResult(content: B)

      class MasterClass(supervisor: ActorRef, system: ActorSystem) extends Actor {
        val slaves: IS[ActorRef] = IS.fill(threadNum)(system.actorOf(Props(new SlaveActor())))
        var started = 0
        var finished = 0
        val tasks: IS[A] = seq.toIndexedSeq
        var results: List[B] = List()

        def getNextTask(): EvalTask = {
          val t = tasks(started)
          started += 1
          EvalTask(t)
        }

        def receive: PartialFunction[Any, Unit] = {
          case StartTask =>
            (0 until math.min(threadNum, tasks.length)).foreach { case i =>
              slaves(i) ! getNextTask()
            }
          case EvalResult(b) =>
            results = b :: results
            finished += 1
            if(started < tasks.length){
              sender() ! getNextTask()
            }
            if(finished == tasks.length)
              supervisor ! results.reverse
        }
      }

      class SlaveActor extends Actor {
        def receive: PartialFunction[Any, Unit] = {
          case EvalTask(content) =>
            val r = f(content)
            sender() ! EvalResult(r)
        }
      }

      val system = ActorSystem("parallelMap-system")
      val inbox = Inbox.create(system)
      val master = system.actorOf(Props(new MasterClass(inbox.getRef(), system)))
      master ! StartTask
      val result = inbox.receive(timeoutSec.seconds).asInstanceOf[Seq[B]]
      system.terminate().value
      result
    }else{
      seq.map(f).toIndexedSeq
    }
  }

  def processMap(args: Array[String], tasks: IS[Int], processNum: Int, mainClass: Object)(f: Int => Unit): Unit = {
    if(args.isEmpty){
      parallelMapOrdered(tasks, processNum){ i =>
        println(s"[JOB STARTED] id = $i")

        import java.io.File
        import java.nio.file.Paths
        val javaPath = Paths.get(System.getProperty("java.home"), "bin", "java").toFile.getAbsolutePath
        val jarPath = new File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath).getAbsolutePath


        val mainClassName = {
          val s = mainClass.getClass.getName
          assert(s.last == '$')
          s.init
        }
        println(s"mainClassName = ${mainClassName}")
        val cmd = s"$javaPath -cp $jarPath $mainClassName $i" //be careful not to create fork bombs!
        println(cmd)

        import sys.process._
        cmd.split("\\s+").toSeq.!

        println(s"[JOB FINISHED] id = $i")
      }
    }else{
      f(args.head.toInt)
    }
  }

  def main(args: Array[String]): Unit = {
    parallelMapOrdered(0 until 100, 6){ i =>
//      Thread.sleep(100)
      println(i)
    }
//    processMap(args, 0 until 100, 3, mainClass = this){
//      i => println(s"*** $i ***")
//    }
  }
}
