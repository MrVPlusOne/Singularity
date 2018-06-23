package singularity

import java.awt.image.BufferedImage

import StandardSystem._
import singularity.GeneticOperator.ExprGen

import scala.util.Random

/** Describes a fuzzing task. Only use this if you want to specify problem-specific [[GPEnvironment]].
  * Otherwise, use [[singularity.ProblemConfig]] instead.
  * @see [[singularity.ProblemConfig]]  */
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
    FileInteraction.saveObjectToFile(name+".serialized")(value.toVector)
  }

  def emptyAction(): Unit = ()

  def notPossible[T](): T = throw new Exception("Not possible!")

}
