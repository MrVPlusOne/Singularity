package benchmarks

import edu.utexas.stac.Cost
import patsyn.ProblemConfig
import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem.AccountingWizard.{EmptyCommand, OrderCommand, UpdateHours}
import patsyn._
import patsyn.StandardSystem._
import scala.util.Random

object AccountingWizardFuncHelper {

  case class ServerEnv(workingDir: String, portNum: Int) {
    def cookieFile = s"$workingDir/myCookieFile.txt"

    val curlPrefix = Seq("curl", "-s", "-k", "-L", "-H", "Content-Type: application/json")
    val getMethodPrefix = Seq("-X", "GET")
    val postMethodPrefix = Seq("-X", "POST")
    val putMethodPrefix = Seq("-X", "PUT")
    val genCookiePrefix = Seq("-c", cookieFile)
    val useCookiePrefix = Seq("-b", cookieFile)
    def dataPrefix(data: String) = { Seq("-d", data) }

    val loginSite = Seq(s"https://localhost:$portNum/login")
    val logoutSite = Seq(s"https://localhost:$portNum/logout")
    val expenditureReportSite = Seq(s"https://localhost:$portNum/manager/report/expenditure")
    def createProjectSite(name: String, cap: Int) = {
      Seq(s"https://localhost:$portNum/manager/project/$name/$cap")
    }
    def orderItemSite(name: String) = {
      Seq(s"https://localhost:$portNum/employee/orderItems/$name")
    }

    def wrapExpenditure(cap: Int) = {
      // expenditure must be in the range 1K ~ 3M
      val maxValue = 3000000
      val minValue = 1000
      SimpleMath.wrapInRange(cap, maxValue - minValue) + minValue
    }
  }

  def managerSetup(env: ServerEnv, projectName: String, projectCap: Int) = {
    import scala.sys.process._
    import env._

    (curlPrefix ++ postMethodPrefix ++ genCookiePrefix ++ dataPrefix("""{"username":"Burger.King", "password":"BK4CLGBG"}""") ++ loginSite).!
    (curlPrefix ++ putMethodPrefix ++ useCookiePrefix ++ createProjectSite(projectName, wrapExpenditure(projectCap))).!
    (curlPrefix ++ getMethodPrefix ++ logoutSite).!

    FileInteraction.deleteIfExist(cookieFile)
  }

  def managerQuery(env: ServerEnv) = {
    import scala.sys.process._
    import env._

    (curlPrefix ++ postMethodPrefix ++ genCookiePrefix ++ dataPrefix("""{"username":"Burger.King", "password":"BK4CLGBG"}""") ++ loginSite).!
    (curlPrefix ++ getMethodPrefix ++ useCookiePrefix ++ expenditureReportSite).run( ProcessLogger(line => ()))
    (curlPrefix ++ getMethodPrefix ++ logoutSite).!

    FileInteraction.deleteIfExist(cookieFile)
  }

  def employeeRequest(env: ServerEnv, projectName: String, values: Vector[EValue]) = {
    import scala.sys.process._
    import env._

    (curlPrefix ++ postMethodPrefix ++ genCookiePrefix ++ dataPrefix("""{"username":"Marie.Callender", "password":"MC1CRNCPNC"}""") ++ loginSite).!

    values.map {
      cmd =>
        (curlPrefix ++ postMethodPrefix ++ useCookiePrefix ++ dataPrefix(AccountingWizardExample.cmdToJson(cmd)) ++ orderItemSite(projectName)).run(ProcessLogger(line => ()))
    }

    (curlPrefix ++ getMethodPrefix ++ logoutSite).!

    FileInteraction.deleteIfExist(cookieFile)
  }
}

object AccountingWizardRunningTimeFunc extends RemoteFunc[(Vector[EValue], Int), Option[Long]] {

  def jarPath: String = this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath

  def sendRequests(values: Vector[EValue], workingDir: String, portNum: Int) = {
    import AccountingWizardFuncHelper._
    val env = ServerEnv(workingDir, portNum)

    values match {
      case IS(VectValue(commands), VectValue(pname), IntValue(cap)) => {
        val projectName = FuzzingTaskProvider.vectIntToString(pname).filter(ch => ch != 0)
        managerSetup(env, projectName, cap)
        employeeRequest(env, projectName, commands)
        managerQuery(env)
      }
    }
  }

  def f(input: (Vector[EValue], Int), workingDir: String): Option[Long] = {
    val values = input._1
    val portNum = 4567 + input._2

    Some(BenchmarkSet.measureCost {

      // Initialize the server
      spark.Spark.port(portNum)
      com.bbn.accounting.wizard.AccountingWizard.main(Array())

      // Translate values into requests and send them
      BenchmarkSet.handleException(()) {
        sendRequests(values, workingDir, portNum)

        // Cleanup
        spark.Spark.stop()
        FileInteraction.deleteDirIfExist(".store")
      }
    })
  }
}

object AccountingWizardFileSizeFunc extends RemoteFunc[(Vector[EValue], Int), Option[Long]] {

  def jarPath: String = this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath

  def sendRequests(values: Vector[EValue], workingDir: String, portNum: Int) = {
    import AccountingWizardFuncHelper._
    val env = ServerEnv(workingDir, portNum)

    values match {
      case IS(VectValue(commands), VectValue(pname), IntValue(cap)) => {
        val projectName = FuzzingTaskProvider.vectIntToString(pname).filter(ch => ch != 0)
        managerSetup(env, projectName, cap)
        employeeRequest(env, projectName, commands)
      }
    }
  }

  def f(input: (Vector[EValue], Int), workingDir: String): Option[Long] = {
    val values = input._1
    val portNum = 4567 + input._2

    Cost.reset()

    // Initialize the server
    spark.Spark.port(portNum)
    com.bbn.accounting.wizard.AccountingWizard.main(Array())

    // Translate values into requests and send them
    BenchmarkSet.handleException(()) {
      sendRequests(values, workingDir, portNum)
    }
    val cost = BenchmarkSet.handleException(0L){
      FileInteraction.measureFileSize(".store")
    }

    // Cleanup
    spark.Spark.stop()
    BenchmarkSet.handleException(()) {
      FileInteraction.deleteDirIfExist(".store")
    }

    Some(cost)
  }
}

object AccountingWizardExample {

  val timeSupernova: Supernova = {
    import patsyn.StandardSystem.AccountingWizard._
    new Supernova(
      extendedConstRule = rule => {
        case Command => (r: Random) => componentSet.mkOrder.eval(
          IS(rule(EVect(EInt))(r), rule(EInt)(r), rule(EInt)(r))
        )
      },
      extraSafeFunctions = IS(componentSet.mkOrder),
      extraUnsafeFunctions = IS()
    )
  }

  val spaceSupernova: Supernova = {
    import patsyn.StandardSystem.AccountingWizard._
    new Supernova(
      extendedConstRule = rule => {
        case Command => (r: Random) =>
          if(r.nextDouble()<0.5) {
            componentSet.mkOrder.eval(
              IS(rule(EVect(EInt))(r), rule(EInt)(r), rule(EInt)(r)))
          }else{
            SimpleMath.randomSelect(r)(IS(SwitchLocal, GetHours, GetFileSizes))
          }
      },
      extraSafeFunctions = IS(componentSet.mkOrder) ++ additionalComponents.collection,
      extraUnsafeFunctions = IS()
    )
  }

  def cmdToJson(cmdValue: EValue) = cmdValue match {
    case OrderCommand(itemNameVec, cost, quantity) =>
      val itemName = String.valueOf(itemNameVec.map{i => i.toChar}.filter{ch => ch != 0 }.toArray)
      s"""{"itemName":"$itemName","cost":$cost,"quantity":$quantity}"""
    case UpdateHours(quatity) =>
      s"""{"quantity":"$quatity"}"""
    case _:EmptyCommand =>
      ""
  }

  def timingExample(ioId: Int): ProblemConfig = {

    val workingDir = FileInteraction.getWorkingDir(ioId)

    ProblemConfig(
      "stac.e5.accountingwizard.timing",
      outputTypes = IS(EVect(AccountingWizard.Command), EVect(EInt), EInt),
      sizeF = {
        case IS(VectValue(commands), VectValue(pname), IntValue(cap)) =>
          val setupSize = 250 + pname.length
          val cmdSize = commands.map(c => cmdToJson(c).length + 250).sum
          setupSize + cmdSize
      },
      resourceUsage = values => AccountingWizardRunningTimeFunc((values.toVector, ioId), workingDir).get
    )
  }

  def fileSizeExample(ioId: Int): ProblemConfig = {
    val workingDir = FileInteraction.getWorkingDir(ioId)

    ProblemConfig(
      "stac.e5.accountingwizard.filesize",
      outputTypes = IS(EVect(AccountingWizard.Command), EVect(EInt), EInt),
      sizeF = {
        case IS(VectValue(commands), VectValue(pname), IntValue(cap)) =>
          val setupSize = 250 + pname.length
          val cmdSize = commands.map(c => cmdToJson(c).length + 250).sum
          setupSize + cmdSize
      },
      resourceUsage = values => AccountingWizardFileSizeFunc((values.toVector, ioId), workingDir).get
    )
  }

  def runTimeExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    timeSupernova.fuzzProblem(
      timingExample(seed),
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(10000), timeLimitInMillis = 200000), rand)
  }

  def runSpaceExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    spaceSupernova.fuzzProblem(
      fileSizeExample(seed),
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(10000), timeLimitInMillis = 200000), rand)
  }

  def main(args: Array[String]): Unit = {
    SimpleMath.processMap(
      args,
      0 until 20,
      10,
      this.getClass
    ){
      i => runSpaceExample(i, useGUI = false)
    }
  }
}
