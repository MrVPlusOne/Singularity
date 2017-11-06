package patsyn

import measure.{TimeMeasureExamples, TimeMeasurement, TimeTools}
import patsyn.GeneticOperator.ExprGen
import patsyn.StandardSystem.{EInt, EVect, IntComponents, IntValue, VectComponents, VectValue}
import StandardSystem._

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

case class FuzzingExample(outputTypes: IS[EType],
                          sizeF: PartialFunction[IS[EValue], Int],
                          sizeOfInterest: Int = 500,
                          resourceUsage: PartialFunction[IS[EValue], Double],
                          gpEnv: GPEnvironment,
                          displayValue: IS[EValue] => String = _.toString
                         )

object FuzzingExample{
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

  def insertionSortExample: FuzzingExample = {
    FuzzingExample(
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) =>
          v.length
      },
      resourceUsage = {
        case IS(VectValue(v)) =>
          val c = new Counter()
          ExampleAlgorithms.insertionSort(c)(toIntVect(v))
          c.read()
      },
      gpEnv = sortingEnv
    )
  }

  def quickSortExample: FuzzingExample = {
    def choosePivot(xs: IS[Int]): Int = {
      xs(xs.length/2) // choose middle
      //      xs(xs.length/3) // 1/3
    }

    FuzzingExample(
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) =>
          v.length
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val c = new Counter()
          ExampleAlgorithms.quickSort(c, choosePivot)(toIntVect(vec))
          c.read()
      },
      gpEnv = sortingEnv
    )
  }

  def randomQuickSortExample(seed: Int): FuzzingExample = {
    FuzzingExample(
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) =>
          v.length
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val random = new Random(seed)
          val c = new Counter()
          ExampleAlgorithms.quickSort(c, xs => xs(random.nextInt(xs.length)))(toIntVect(vec))
          c.read()
      },
      gpEnv = sortingEnv
    )
  }

  def listSearchExample: FuzzingExample = {
    FuzzingExample(
      outputTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(VectValue(v), IntValue(_)) => v.length
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(idx)) =>
          val c = new Counter()
          ExampleAlgorithms.listSearchAndCopy(c)(toIntVect(vec), idx)
          c.read()
      },
      gpEnv = sortingEnv
    )
  }

  def vectIntToString(vec: VectValue): String = {
    String.valueOf(vectIntToCharArray(vec))
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

  def phpHashTableExample(timeout: TimeMeasurement.DoubleAsMillis): FuzzingExample = {
    val example = TimeMeasureExamples.phpHashExampleNoFile
    FuzzingExample(
      outputTypes = IS(EVect(EVect(EInt))),
      sizeF = {
        case IS(VectValue(strings)) =>
          strings.map(s => s.asInstanceOf[VectValue].value.length).sum
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val strings = vec.map(v => vectIntToString(v.asInstanceOf[VectValue]))
          example.measure(strings, timeout)
      },
      gpEnv = phpHashEnv
    )
  }

  def phpHashCollision: FuzzingExample = {
    FuzzingExample(
      outputTypes = IS(EVect(EVect(EInt))),
      sizeF = {
        case IS(VectValue(strings)) =>
          strings.map(s => s.asInstanceOf[VectValue].value.length).sum
      },
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
  def regexExample(regex: String, regexDic: Int => String): FuzzingExample = {
    val pattern = Pattern.compile(regex)
    FuzzingExample(
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(chars)) => chars.length
      },
      sizeOfInterest = 200,
      resourceUsage = {
        case IS(VectValue(chars)) =>
          val counter = new Counter()
          val s = chars.asInstanceOf[Vector[IntValue]].flatMap(i => regexDic(i.value))
          pattern.matcher(counter, s).find()
          counter.read()
      },
      gpEnv = abcRegexEnv,
      displayValue = { case IS(VectValue(vs)) =>
        vs.map(intValueAsChar).mkString("")
      }
    )
  }

  def ccs_hashFunc(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = 5381
    for(i <- cs.indices){
      hash = ((hash << 5) + hash) + cs(i)
    }
    hash
  }

  def gabFeed2_hashCollisionExample: FuzzingExample = {
    FuzzingExample(
      outputTypes = IS(EVect(EVect(EInt))),
      sizeF = {
        case IS(VectValue(strings)) =>
          strings.map(s => s.asInstanceOf[VectValue].value.length).sum
      },
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

  def graphAnalyzerExample: FuzzingExample = {
    import java.io.FileWriter

    import user.commands.CommandProcessor
    import edu.utexas.stac.Cost

    FuzzingExample(
      outputTypes = IS(EVect(EVect(EInt))),
      sizeF = {
        case IS(VectValue(graphs)) =>
          graphs.map(g => g.asInstanceOf[VectValue].value.length+1).sum
      },
      sizeOfInterest = 12,
      resourceUsage = {
        case IS(VectValue(graphs)) =>

          val fileContent = graphs.zipWithIndex.map{
            case (graphVec, i) =>
              val vec = graphVec.asInstanceOf[VectValue].value.map{e => e.asInstanceOf[IntValue].value}
              FuzzingExample.intVectorToGraph(i, vec)
          }.mkString("\n")

          val fw = new FileWriter("input-files/genGraph.dot")
          fw.write(fileContent)
          fw.close()

          Cost.write(0L)

          val timeLimit = 10*1000
          try {
            import scala.concurrent._
            import scala.concurrent.duration._
            import scala.concurrent.ExecutionContext.Implicits.global

            Await.result(Future(
              CommandProcessor.main("dot input-files/genGraph.dot xy diagram png output-files/PNG_output.png".split(" "))
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
