package patsyn

import java.awt.image.BufferedImage

import edu.utexas.stac.Cost
import patsyn.GeneticOperator.ExprGen
import patsyn.StandardSystem._

import scala.util.Random


trait FuzzingTaskProvider {
  protected def task: RunningFuzzingTask

  def outputTypes: IS[EType]

  def sizeF: PartialFunction[IS[EValue], Int]

  def setupTask(task: RunningFuzzingTask): Unit = {}

  def teardownTask(task: RunningFuzzingTask): Unit = {}

  def displayValue: PartialFunction[IS[EValue], String] = FuzzingTaskProvider.defaultDisplayValue

  def saveValueWithName(value: IS[EValue], name: String): Unit = {
    FuzzingTaskProvider.defaultSaveValueWithName(value, name)
  }

  def run[A](f: RunningFuzzingTask => A): A = {
    val task = this.task
    try {
      setupTask(task)
      f(task)
    } finally {
      teardownTask(task)
    }
  }

  def runAsProbConfig[A](name: String)(f: ProblemConfig => A): A = {
    run{ task =>
      val config = ProblemConfig(name, outputTypes, task.resourceUsage, sizeF, displayValue, saveValueWithName)
      f(config)
    }
  }
}


case class ResourceConfig(resourceUsage: PartialFunction[IS[EValue], Double], setup: () => Unit, teardown: () => Unit)

case class RunningFuzzingTask(sizeOfInterest: Int = 500,
                              resourceUsage: PartialFunction[IS[EValue], Double],
                              gpEnv: GPEnvironment
                             )

//noinspection TypeAnnotation
object FuzzingTaskProvider {

  def defaultDisplayValue: PartialFunction[IS[EValue], String] = {
    case v => v.toString
  }

  def defaultSaveValueWithName(value: IS[EValue], name: String): Unit = {
    import java.io._
    val fw = new FileWriter(new File(name + ".txt"))
    try {
      fw.write(defaultDisplayValue(value))
    } finally {
      fw.close()
    }
    FileInteraction.saveObjectToFile(name+".serialized")(value.head)
  }

  def emptyAction(): Unit = ()

  def notPossible[T](): T = throw new Exception("Not possible!")

  def toIntVect(v: Vector[EValue]): Vector[Int] = {
    v.asInstanceOf[Vector[IntValue]].map(_.value)
  }

  def makeConstMap(pairs: (EType, Random => EValue)*): Map[EType, ExprGen[EConst]] = {
    pairs.map { case (t, f) =>
      t -> ExprGen(t, r => EConst(t, f(r)))
    }.toMap
  }

  def sortingEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> (r => r.nextInt(12)),
      EVect(EInt) -> (_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def vectIntToString(vec: VectValue, charSize: Int = 16): String = {
    String.valueOf(vectIntToCharArray(vec, charSize).toArray)
  }

  def vectIntToCharArray(vec: VectValue, charSize: Int): List[Char] = {
    val range = 1 << charSize
    vec.value.map { i =>
      SimpleMath.wrapInRange(i.asInstanceOf[IntValue].value, range).toChar
    }.toList
  }

  def hashEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> (r => r.nextInt(12)),
      EVect(EInt) -> (_ => Vector()),
      EVect(EVect(EInt)) -> (_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes ++ stateTypes)
  }

  def hashCollisionExample(hashFunc: Seq[Char] => Int, charSize: Int) = new FuzzingTaskProvider {
    def totalCollisionMetric(vec: Vector[EValue]): Double = {
      val hashes = vec.map(v => {
        vectIntToCharArray(v.asInstanceOf[VectValue], charSize)
      }).distinct.map(hashFunc)

      hashes.groupBy(identity).values.map {
        elems => elems.length - 1
      }.sum
    }

    def squareMetric(vec: Vector[EValue]): Double = {
      val hashes = vec.map(v => {
        vectIntToCharArray(v.asInstanceOf[VectValue], charSize).takeWhile(ch => ch.toInt != 0)
      }).distinct.map(hashFunc)

      hashes.groupBy(identity).values.map{g =>
        val c = g.length - 1
        c*c
      }.sum
    }

    def outputTypes = IS(EVect(EVect(EInt)))


    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      resourceUsage = {
        case IS(VectValue(vec)) =>
          squareMetric(vec)
      },
      gpEnv = hashEnv
    )

    def sizeF = {
      case IS(VectValue(strings)) =>
        strings.map(s => s.asInstanceOf[VectValue].value.length + 1).sum
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      println(s"# of string = ${value(0).asInstanceOf[VectValue].value.length}")
    }
  }

  def phpHashCollisionExample = hashCollisionExample(HashFunc.php, 8)

  def javaHashCollisionExample = hashCollisionExample(HashFunc.java, 16)

  def rubyHashCollisionExample = hashCollisionExample(HashFunc.ruby, 8)

  def aspDotNetHashCollisionExample = hashCollisionExample(HashFunc.aspDotNet, 16)

  def pythonHashCollisionExample = hashCollisionExample(HashFunc.python, 8)

  def v8HashCollisionExample = hashCollisionExample(HashFunc.v8, 8)

  def murmur2HashCollisionExample = hashCollisionExample(HashFunc.murmur2(0), 8)


  private def hashPerformanceExample(hashFunc: Array[String] => Any, charSize: Int) = new FuzzingTaskProvider {
    def outputTypes = IS(EVect(EVect(EInt)))


    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val strs = vec.map(v => vectIntToString(v.asInstanceOf[VectValue], charSize)).filter(s => s.nonEmpty).toArray

          Cost.reset()
          hashFunc(strs)
          Cost.read()
      },
      gpEnv = hashEnv
    )

    def sizeF = {
      case IS(VectValue(strings)) =>
        strings.map(s => s.asInstanceOf[VectValue].value.length + 1).sum
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      println(s"# of string = ${value(0).asInstanceOf[VectValue].value.length}")
    }
  }

  def escapeStrings(s: String): String = {
    import org.apache.commons.lang3.StringEscapeUtils

    StringEscapeUtils.escapeJava(s)
  }
}
