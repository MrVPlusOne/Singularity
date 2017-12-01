package cli

import patsyn.{RunConfig, Sledgehammer}
import scopt.OptionParser

case class JarTesterOption(jarPath: String = "",
                           className: String = "",
                           methodName: String = "",
                           ioId: Int = 0,
                           seed: Int = 0,
                           disableGui: Boolean = false)

object JarTester {
  import java.io.File
  import java.net.URLClassLoader

  private def getRunConfig(option: JarTesterOption): RunConfig = {
    val defaultConfig = RunConfig.default
    defaultConfig.copy(
      benchConfig = defaultConfig.benchConfig.copy(
        ioId = option.ioId
      ),
      execConfig = defaultConfig.execConfig.copy(
        randomSeed = option.seed,
        useGUI = !option.disableGui
      )
    )
  }

  def runJarTester(opt: JarTesterOption): Unit = {
    var classLoader = new URLClassLoader(
      Array(new File(opt.jarPath).toURI.toURL), this.getClass.getClassLoader)
    val targetClass = classLoader.loadClass(opt.className)
    val intArrayClass = Class.forName("[I")
    val method = targetClass.getDeclaredMethod(opt.methodName, intArrayClass)
    if (method == null)
      throw new RuntimeException(s"Cannot find method ${opt.methodName} of class ${opt.className} in jar file ${opt
        .jarPath}")
    val runConfig = getRunConfig(opt)

    val taskProvider = new patsyn.FuzzingTaskProvider {
      import patsyn.FuzzingTaskProvider._
      import patsyn.StandardSystem._
      import patsyn._
      import edu.utexas.stac.Cost

      override def sizeF: PartialFunction[IS[EValue], Int] = {
        case IS(VectValue(v)) => v.length
      }

      def outputTypes = IS(EVect(EInt))

      override protected def task: RunningFuzzingTask = RunningFuzzingTask(
        resourceUsage = {
          case IS(VectValue(vec)) =>
            val intArray = toIntVect(vec).toArray
            Cost.reset()
            method.invoke(null, intArray)
            Cost.read()
        },
        gpEnv = sortingEnv
      )
    }
    Sledgehammer.sledgehammerRun(taskProvider, runConfig)
  }

  def main(args: Array[String]): Unit = {
    val parser = jarTesterOptParser()
    parser.parse(args, JarTesterOption()).foreach(runJarTester)
  }

  def jarTesterOptParser(): OptionParser[JarTesterOption] = {
    new scopt.OptionParser[JarTesterOption]("patsyn_jar_tester") {
      head("patsyn Jar Tester", "0.1")

      opt[Unit]('n', "no-gui").action((_, c) =>
        c.copy(disableGui = true)).text("Disable the GUI panel.")

      opt[Int]('i', "ioId").action((id, c) =>
        c.copy(ioId = id)
      ).text("The id used to perform IO actions. If you have n processes running at the same time, just set their idIo to 0 through n.")

      opt[Int]('s', "seed").action((s, c) =>
        c.copy(seed = s)).text("The random seed to use. Default to 0.")

      help("help").text("Prints this usage text")
      version("version").text("Prints the version info")

      override def showUsageOnError = true

      arg[String]("<jar_file>").required().action((x, c) =>
        c.copy(jarPath = x)).text("Jar file to test.")
      arg[String]("<class_name>").required().action((x, c) =>
        c.copy(className = x)).text("Class name to test.")
      arg[String]("<method_name>").required().action((x, c) =>
        c.copy(methodName = x)).text("Method name to test.")
    }
  }
}
