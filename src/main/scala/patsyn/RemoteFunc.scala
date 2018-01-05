package patsyn

trait RemoteFunc[A <: Serializable, B <: Serializable] {
  def tempFileName = "tempFile.serialized"

  def jarPath: String

  def f(input: A, workingDir: String): B

  def apply(input: A, workingDir: String) = {
    import java.nio.file._

    val tempFile = s"$workingDir/$tempFileName"
    FileInteraction.saveObjectToFile(tempFile)((false, input))

    val javaPath = Paths.get(System.getProperty("java.home"), "bin", "java").toFile.getAbsolutePath
//    val jarPath = new File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath).getAbsolutePath

    val mainClassName = {
      val s = this.getClass.getName
      assert(s.last == '$')
      s.init
    }
    val cmd = s"$javaPath -cp $jarPath $mainClassName $workingDir" //be careful not to create fork bombs!

    import sys.process._
    cmd.split("\\s+").toSeq.!
    val (true, result) = FileInteraction.readObjectFromFile[(Boolean,B)](tempFile)
    result
  }

  def main(args: Array[String]): Unit = {
    val workingDir = args(0)
    val tempFile = s"$workingDir/$tempFileName"
    val (false, input) = FileInteraction.readObjectFromFile[(Boolean,A)](tempFile)
    val result = f(input, workingDir)
    FileInteraction.saveObjectToFile(tempFile)((true, result))
    System.exit(0)
  }
}

object ReverseList extends RemoteFunc[List[Int], List[Int]] {

  def f(xs: List[Int], workingDir: String) = xs.reverse

  def jarPath: String = "target/scala-2.12/PatternSynthBenchmarkDriver.jar"
}

object TestRemoteFunc{
  def main(args: Array[String]): Unit = {
    val workingDir = FileInteraction.getWorkingDir(0)
    println{
      ReverseList(List(1,2,3,4,5), workingDir)
    }
  }
}