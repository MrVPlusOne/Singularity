package patsyn

import java.awt.image.BufferedImage
import java.io.FileOutputStream
import java.util.zip.ZipOutputStream

import edu.utexas.stac.Cost
import patsyn.GeneticOperator.ExprGen
import patsyn.StandardSystem.{EInt, EVect, IntComponents, IntValue, VectComponents, VectValue, _}

import scala.concurrent.TimeoutException
import scala.util.Random


class Counter{

  private var counter = 0L

  def count(): Unit = {
    counter += 1
  }

  def count(n: Int): Unit = {
    counter += n
  }

  def read(): Double = {
    counter.toDouble
  }
}


trait FuzzingTaskProvider{
  protected def task: RunningFuzzingTask

  def sizeF: PartialFunction[IS[EValue], Int]

  def setupTask(task: RunningFuzzingTask): Unit = {}

  def teardownTask(task: RunningFuzzingTask): Unit = {}

  def displayValue: PartialFunction[IS[EValue], String] = {
    case v => v.toString
  }

  def saveValueWithName(value: IS[EValue], name: String): Unit = {
    import java.io._
    val fw = new FileWriter(new File(name+".txt"))
    try{
      fw.write(displayValue(value))
    } finally {
      fw.close()
    }
  }

  def run[A](f: RunningFuzzingTask => A): A = {
    val task = this.task
    try{
      setupTask(task)
      f(task)
    }finally{
      teardownTask(task)
    }
  }
}


case class RunningFuzzingTask(outputTypes: IS[EType],
                              sizeOfInterest: Int = 500,
                              resourceUsage: PartialFunction[IS[EValue], Double],
                              gpEnv: GPEnvironment
                      )

//noinspection TypeAnnotation
object FuzzingTaskProvider{

  def emptyAction(): Unit = ()

  def notPossible[T](): T = throw new Exception("Not possible!")

  def toIntVect(v: Vector[EValue]): Vector[Int] = {
    v.asInstanceOf[Vector[IntValue]].map(_.value)
  }

  def makeConstMap(pairs: (EType, IS[Random => EValue])*): Map[EType, IS[ExprGen[EConst]]] = {
    pairs.map{ case (t, fs) =>
      t -> fs.map(f =>ExprGen(t, r => EConst(t, f(r))))
    }.toMap
  }

  def sortingEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def insertionSortExample = new FuzzingTaskProvider {
    def sizeF = {
      case IS(VectValue(v)) => v.length
    }

    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      outputTypes = IS(EVect(EInt)),
      resourceUsage = {
        case IS(VectValue(v)) =>
          val c = new Counter()
          ExampleAlgorithms.insertionSort(c)(toIntVect(v))
          c.read()
      },
      gpEnv = sortingEnv
    )
  }

  def quickSortExample = new FuzzingTaskProvider {
    def choosePivot(xs: IS[Int]): Int = {
      xs(xs.length/2) // choose middle
      //      xs(xs.length/3) // 1/3
    }

    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      outputTypes = IS(EVect(EInt)),
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val c = new Counter()
          ExampleAlgorithms.quickSort(c, choosePivot)(toIntVect(vec))
          c.read()
      },
      gpEnv = sortingEnv
    )

    def sizeF = {
      case IS(VectValue(v)) =>
        v.length
    }
  }

  def randomQuickSortExample(seed: Int) = new FuzzingTaskProvider {
    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      outputTypes = IS(EVect(EInt)),
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val random = new Random(seed)
          val c = new Counter()
          ExampleAlgorithms.quickSort(c, xs => xs(random.nextInt(xs.length)))(toIntVect(vec))
          c.read()
      },
      gpEnv = sortingEnv
    )

    def sizeF = {
      case IS(VectValue(v)) =>
        v.length
    }
  }

  def listSearchExample = new FuzzingTaskProvider {
    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      outputTypes = IS(EVect(EInt), EInt),
      resourceUsage = {
        case IS(VectValue(vec), IntValue(idx)) =>
          val c = new Counter()
          ExampleAlgorithms.listSearchAndCopy(c)(toIntVect(vec), idx)
          c.read()
      },
      gpEnv = sortingEnv
    )

    def sizeF = {
      case IS(VectValue(v), IntValue(_)) => v.length
    }
  }

  def vectIntToString(vec: VectValue): String = {
    String.valueOf(vectIntToCharArray(vec).toArray)
  }

  def vectIntToCharArray(vec: VectValue): List[Char] = {
    vec.value.map { i =>
      (i.asInstanceOf[IntValue].value % 256).toChar
    }.toList
  }

  def phpHashEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector()),
      EVect(EVect(EInt)) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def phpHashCollision = new FuzzingTaskProvider {
    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      outputTypes = IS(EVect(EVect(EInt))),
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val hashes = vec.map(v => {
            vectIntToCharArray(v.asInstanceOf[VectValue])
          }).distinct.map(ccs_hashFunc)

          hashes.groupBy(identity).values.map{
            elems => elems.length - 1
          }.sum
      },
      gpEnv = phpHashEnv
    )

    def sizeF = {
      case IS(VectValue(strings)) =>
        strings.map(s => s.asInstanceOf[VectValue].value.length).sum
    }
  }

  def toLowercase(i: Int): Char = {
    ('a' + i % 26).toChar
  }

  def abcRegexEnv: GPEnvironment = {
    val symbols = "abcABC ,.\\(){}[]+-*/=_".toCharArray.toIndexedSeq
    val intGens: IS[Random => IntValue] = IS(
      r => r.nextInt(7),
      r => SimpleMath.randomSelect(r)(symbols).toInt
    )

    val constMap = makeConstMap(
      EInt -> intGens,
      EVect(EInt) -> IS(_ => Vector()),
      //      EVect(EVect(EInt)) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def intValueAsChar(eValue: EValue): Char = {
    eValue.asInstanceOf[IntValue].value.toChar
  }

  import regex._
  def regexExample(regex: String, regexDic: Int => String) = new FuzzingTaskProvider {
    val pattern = Pattern.compile(regex)


    override def displayValue = {
      case IS(VectValue(vs)) =>
      vs.map(intValueAsChar).mkString("")
    }

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = 200,
        resourceUsage = {
          case IS(VectValue(chars)) =>
            val counter = new Counter()
            val s = chars.asInstanceOf[Vector[IntValue]].flatMap(i => regexDic(i.value))
            pattern.matcher(counter, s).find()
            counter.read()
        },
        gpEnv = abcRegexEnv
      )
    }

    def sizeF = {
      case IS(VectValue(chars)) => chars.length
    }
  }

  def ccs_hashFunc(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = 5381
    for(i <- cs.indices){
      hash = ((hash << 5) + hash) + cs(i)
    }
    hash
  }

  def gabFeed2_hashCollisionExample = new FuzzingTaskProvider {
    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EVect(EVect(EInt))),
        resourceUsage = {
          case IS(VectValue(strings)) =>
            import gabfeed2.hashmap._
            val map = new HashMap[String, Int]();
            strings.foreach{
              case VectValue(chars) =>
                val string = chars.map{ case IntValue(i) => toLowercase(i)}.mkString("")
                map.put(string, 0)
            }
            map.resizeNum.toDouble
        },
        gpEnv = phpHashEnv
      )
    }

    def sizeF = {
      case IS(VectValue(strings)) =>
        strings.map(s => s.asInstanceOf[VectValue].value.length).sum
    }
  }

  def intVectorToGraph(graphId: Int, refs: Vector[Int]): String = {
    val graphName = if(graphId == 0) "main" else s"L$graphId"
    val graphBody = refs.zipWithIndex.map { case (ref, i) =>
      val edgeName = s"$graphName-$i"
      val label = if(ref == graphId) "net" else s"container:L$ref"
      s"""    $edgeName [type="$label", color="red", x=30, y=30, h=30, w=30];"""
    }.mkString("\n")
    s"""graph "$graphName" {
       |$graphBody
       |}
     """.stripMargin
  }

  def graphAnalyzerExample(workingDir: String) = new FuzzingTaskProvider {

    def sizeF = {
      case IS(VectValue(graphs)) =>
        graphs.map(g => g.asInstanceOf[VectValue].value.length+1).sum
    }

    protected def task: RunningFuzzingTask = {
      import java.io.FileWriter

      import edu.utexas.stac.Cost
      import user.commands.CommandProcessor

      RunningFuzzingTask(
        outputTypes = IS(EVect(EVect(EInt))),
        sizeOfInterest = 12,
        resourceUsage = {
          case IS(VectValue(graphs)) =>

            val fileContent = graphs.zipWithIndex.map{
              case (graphVec, i) =>
                val vec = graphVec.asInstanceOf[VectValue].value.map{e => e.asInstanceOf[IntValue].value}
                intVectorToGraph(i, vec)
            }.mkString("\n")

            val fw = new FileWriter(s"$workingDir/genGraph.dot")
            fw.write(fileContent)
            fw.close()

            Cost.write(0L)

            val timeLimit = 10*1000
            try {
              import scala.concurrent.ExecutionContext.Implicits.global
              import scala.concurrent._
              import scala.concurrent.duration._

              Await.result(Future(
                CommandProcessor.main(s"dot $workingDir/genGraph.dot xy diagram png output-files/PNG_output.png".split(" "))
              ), timeLimit.milliseconds)

              Cost.read().toDouble
            } catch {
              case _: NullPointerException =>
                Cost.read().toDouble
              case _: TimeoutException =>
                println("Timed out!")
                System.exit(1)
                throw new Exception("Timed out!")
            }
        },
        gpEnv = phpHashEnv
      )
    }
  }


  /** Http request example */
  def bloggerExample = new FuzzingTaskProvider {
    import fi.iki.elonen.JavaWebServer

    import sys.process._
    val server = new JavaWebServer(8080)

    override def setupTask(task: RunningFuzzingTask): Unit = {
      new Thread(() => server.start()).start()
    }

    override def teardownTask(task: RunningFuzzingTask): Unit = {
      server.stop()
    }


    override def displayValue = {
      case IS(vec:VectValue)=> vectIntToString(vec)
    }

    def sizeF = {
      case IS(VectValue(chars)) =>
        chars.length
    }

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = 100,
        resourceUsage = {
          case IS(vec :VectValue) =>
            val s = vectIntToString(vec)
            Cost.reset()
            try{
              s"curl -i http://localhost:8080/$s" !
            } catch {
              case _: java.io.IOException =>
            }
            Cost.read().toDouble
        },
        gpEnv = sortingEnv
      )
    }
  }

  def imageEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection ++ IS(IntComponents.shiftByteLeft)

    val stateTypes = constMap.keys.toIndexedSeq ++ IS(EInt, EInt)
    GPEnvironment(constMap, functions, stateTypes)
  }

  def intsToImage(imageWidth: Int, imageHeight: Int, name: String, data: IS[Int]): BufferedImage = {
    import java.awt.image.BufferedImage

    val imageDataSize = imageWidth * imageHeight
    val dataSize = data.length
    val content = (0 until imageDataSize).flatMap { i =>
      val intValue = data(SimpleMath.wrapInRange(i, dataSize))
      IS((intValue >> 24) & 255, intValue & 255, (intValue >> 8) & 255, (intValue >> 16) & 255)
    }
    val image = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_ARGB)
    image.getRaster.setPixels(0, 0, imageWidth, imageHeight, content.toArray)
    image
  }

  def parseCost(s: String): Long = {
    val Array(first, costS) = s.split("\\s+")
    assert(first == "[COST]")
    costS.toLong
  }

  def imageExample(imageWidth: Int, imageHeight: Int, workingDir: String) = new FuzzingTaskProvider {
    import java.io.File
    import javax.imageio.ImageIO

    import sys.process._

    def sizeF = {
      case IS(VectValue(data)) => data.length
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      val data = value.head
      val width = math.max(1, math.sqrt(data.size).toInt)
      val image = intsToImage(width, width, name,
        data.asInstanceOf[VectValue].value.map(_.asInstanceOf[IntValue].value))
      ImageIO.write(image, "png", new File(name + ".png"))
    }

    protected def task: RunningFuzzingTask = {
      var bestPerformanceSoFar = Double.MinValue

      val imageDataSize = imageWidth * imageHeight
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = imageDataSize,
        resourceUsage = {
          case IS(VectValue(data)) =>
            if(data.isEmpty) 0.0
            else {
              val imageName = s"$workingDir/genImage"
              val imagePath  = imageName + ".png"
              val image = intsToImage(imageWidth, imageHeight, imageName, data.map(_.asInstanceOf[IntValue].value))
              ImageIO.write(image, "png", new File(imagePath))

              val jarPath = "benchmarks/image_processor/challenge_program/ipchallenge-ins.jar"
              val results = Seq("java", "-Xint", "-jar", s"$jarPath", "cluster", imagePath).lineStream
              val cost = parseCost(results.last)
              val performance = cost.toDouble
              if (performance > bestPerformanceSoFar) {
                ImageIO.write(image, "png", new File(s"$workingDir/bestImageSoFar.png"))
                bestPerformanceSoFar = performance
              }
              performance
            }
        },
        gpEnv = imageEnv
      )
    }
  }

  def escapeStrings(s: String): String = {
    import org.apache.commons.lang3.StringEscapeUtils

    StringEscapeUtils.escapeJava(s)
  }

  def textCrunchrExample(workDir: String) = new FuzzingTaskProvider {
    import java.util.zip._
    import java.io._
    import sys.process._

    def outputAsZip(outPath: String, content: String, contentFileName: String = "content.txt") = {
      val f = new File(outPath)
      val zipStream = new ZipOutputStream(new FileOutputStream(f))
      val entry = new ZipEntry(contentFileName)
      zipStream.putNextEntry(entry)

      zipStream.write(content.getBytes)
      zipStream.close()
    }

    def sizeF: PartialFunction[IS[EValue], Int] = {
      case IS(VectValue(chars)) => chars.length
    }

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = 200,
        resourceUsage = {
          case IS(VectValue(chars)) =>
            val content = vectIntToString(chars)
            val zipPath = s"$workDir/input.zip"
            outputAsZip(zipPath, content)

            val cost = parseCost(Seq("java", "-Xint", "-cp", "benchmarks/textChrunchr/textChrunchr_3.jar",
              "com.cyberpointllc.stac.host.Main", zipPath).lineStream.last)

            println(s"Cost = $cost")
            cost.toDouble
        },
        gpEnv = abcRegexEnv.copy(stateTypes = abcRegexEnv.stateTypes ++ IS(EInt, EVect(EInt)))
      )
    }


    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      value match {
        case IS(VectValue(chars)) =>
          val content = vectIntToString(chars)
          val zipPath = name+".zip"
          outputAsZip(zipPath, content)
      }
    }
  }
}

object ExampleAlgorithms {

  def quickSort(c: Counter, pivotChoose: (IS[Int]) => Int)(xs: IS[Int]): IS[Int] = {
    c.count()
    if(xs.length < 2) return xs

    val pivot = pivotChoose(xs)
    val left = xs.filter(_ < pivot)
    val right = xs.filter(_ > pivot)
    val middle = xs.filter(_ == pivot)
    c.count(xs.length)
    quickSort(c, pivotChoose)(left) ++ middle ++ quickSort(c, pivotChoose)(right)
  }

  def insertionSort(c: Counter)(xs: IS[Int]): IS[Int] = {
    val array = xs.toArray

    def sortPart(n: Int): Unit = {
      val x = array(n)
      for(i <- n-1 to 0 by -1){
        c.count(2)
        array(i+1) = array(i)

        c.count()
        if(array(i)<=x){
          c.count()
          array(i) = x
          return
        }
      }
      c.count()
      array(0) = x
    }
    for(i <- xs.indices){
      c.count()
      sortPart(i)
    }
    array
  }

  def listSearchAndCopy(c: Counter)(xs: IS[Int], e: Int): Option[IS[Int]] = {
    for(i <- xs.indices){
      c.count(2)
      if(xs(i) == e){
        c.count(i+1)
        return Some(xs.take(i))
      }
    }
    c.count()
    None
  }



  def main(args: Array[String]): Unit = {
    val c = new Counter()
    val rand = new Random(1)
    val r = quickSort(c, _.last)(0 until 20)
    println(r)
    println(c.read())
  }
}
