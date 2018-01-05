package benchmarks

import java.util.Random

import edu.utexas.stac.Cost
import patsyn.ProblemConfig
import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem.AccountingWizard.{EmptyCommand, OrderCommand, UpdateHours}
import patsyn._
import patsyn.StandardSystem._

object ExternalFunc extends RemoteFunc[(Vector[EValue], Int), Option[Long]] {

  def jarPath: String = this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath

  def sendRequests(values: Vector[EValue], workingDir: String, portNum: Int) = {

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

    def managerSetup(projectName: String, projectCap: Int) = {

      import scala.sys.process._

      (curlPrefix ++ postMethodPrefix ++ genCookiePrefix ++ dataPrefix("""{"username":"Burger.King", "password":"BK4CLGBG"}""") ++ loginSite).!
      (curlPrefix ++ putMethodPrefix ++ useCookiePrefix ++ createProjectSite(projectName, wrapExpenditure(projectCap))).!
      (curlPrefix ++ getMethodPrefix ++ logoutSite).!

      FileInteraction.deleteIfExist(cookieFile)
    }

    def managerQuery() = {
      import scala.sys.process._

      (curlPrefix ++ postMethodPrefix ++ genCookiePrefix ++ dataPrefix("""{"username":"Burger.King", "password":"BK4CLGBG"}""") ++ loginSite).!
      (curlPrefix ++ getMethodPrefix ++ useCookiePrefix ++ expenditureReportSite).run( ProcessLogger(line => ()))
      (curlPrefix ++ getMethodPrefix ++ logoutSite).!

      FileInteraction.deleteIfExist(cookieFile)
    }

    def employeeRequest(projectName: String, values: Vector[EValue]) = {
      import scala.sys.process._

      (curlPrefix ++ postMethodPrefix ++ genCookiePrefix ++ dataPrefix("""{"username":"Marie.Callender", "password":"MC1CRNCPNC"}""") ++ loginSite).!

      values.map {
        cmd =>
          (curlPrefix ++ postMethodPrefix ++ useCookiePrefix ++ dataPrefix(AccountingWizardExample.cmdToJson(cmd)) ++ orderItemSite(projectName)).run(ProcessLogger(line => ()))
      }

      (curlPrefix ++ getMethodPrefix ++ logoutSite).!

      FileInteraction.deleteIfExist(cookieFile)
    }

    values match {
      case IS(VectValue(commands), VectValue(pname), IntValue(cap)) => {
        val projectName = FuzzingTaskProvider.vectIntToString(pname).filter(ch => ch != 0)
        managerSetup(projectName, cap)
        employeeRequest(projectName, commands)
        managerQuery()
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
    sendRequests(values, workingDir, portNum)

    // Cleanup
    spark.Spark.stop()
    try {
      FileInteraction.deleteDirIfExist(".store")
    } catch {
      case _: Exception => ()
    }

    val cost: Long = Cost.read()
    Some(cost)
  }
}

object AccountingWizardExample {

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
      resourceUsage = values => ExternalFunc((values.toVector, ioId), workingDir).get
    )
  }

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.accountingWizard.fuzzProblem(
      timingExample(seed),
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(15000)), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(args(0).toInt, false)
  }
}
