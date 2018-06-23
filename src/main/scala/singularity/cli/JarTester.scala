package singularity.cli

import singularity.AllConfigs
import scopt.OptionParser

import scala.util.Random

case class JarTesterOption(jarPath: String = "",
                           className: String = "",
                           methodName: String = "",
                           ioId: Int = 0,
                           seed: Int = 0,
                           disableGui: Boolean = false)

object JarTester {
  import java.io.File
  import java.net.URLClassLoader

  private def getRunConfig(option: JarTesterOption): AllConfigs = {
    val defaultConfig = AllConfigs.default
    defaultConfig.copy(
      runnerConfig = defaultConfig.runnerConfig.copy(
        ioId = option.ioId,
        randomSeed = option.seed,
        useGUI = !option.disableGui)
    )
  }

  def runJarTester(opt: JarTesterOption): Unit = {
    val classLoader = new URLClassLoader(
      Array(new File(opt.jarPath).toURI.toURL), this.getClass.getClassLoader)
    val targetClass = classLoader.loadClass(opt.className)
    val intArrayClass = Class.forName("[I")
    val method = targetClass.getDeclaredMethod(opt.methodName, intArrayClass)
    if (method == null)
      throw new RuntimeException(s"Cannot find method ${opt.methodName} of class ${opt.className} in jar file ${opt
        .jarPath}")

    val runConfig = getRunConfig(opt)

    import singularity.StandardSystem._
    import singularity._
    import benchmarks.ExampleAlgorithms.vectIntToString

    val rand = new Random(runConfig.runnerConfig.randomSeed)

    def squareMetric(vec: Vector[EValue]): Double = {
      val hashes = vec.map(v => {
        vectIntToString(v.asInstanceOf[VectValue])
      }).distinct.map(str =>
        method.invoke(null, str)
      )

      hashes.groupBy(identity).values.map{g =>
        val c = g.length - 1
        c*c
      }.sum
    }

   Supernova.standardSupernova.fuzzProblem(
      problemConfig = ProblemConfig(
        problemName = method.getName,
        outputTypes = IS(EVect(EVect(EInt))),
        sizeF = {
          case IS(VectValue(strings)) =>
            strings.map(s => s.asInstanceOf[VectValue].value.length + 1).sum
        },
        resourceUsage = {
          case IS(VectValue(vec)) =>
            squareMetric(vec)
        },
        displayValue = FuzzingTaskProvider.defaultDisplayValue,
        saveValueWithName = FuzzingTaskProvider.defaultSaveValueWithName
      ),
      execConfig = runConfig.execConfig,
      runnerConfig = runConfig.runnerConfig,
      rand = rand
    )
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
