package patsyn

trait RemoteFunc[A <: Serializable, B <: Serializable] extends (A => B) {
  def tempFile: String

  def jarPath: String

  def f(input: A): B

  def apply(input: A) = {
    import java.nio.file.Paths

    FileInteraction.saveObjectToFile(tempFile)((false, input))

    val javaPath = Paths.get(System.getProperty("java.home"), "bin", "java").toFile.getAbsolutePath
//    val jarPath = new File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath).getAbsolutePath

    val mainClassName = {
      val s = this.getClass.getName
      assert(s.last == '$')
      s.init
    }
    val cmd = s"$javaPath -cp $jarPath $mainClassName" //be careful not to create fork bombs!

    import sys.process._
    cmd.split("\\s+").toSeq.!
    val (true, result) = FileInteraction.readObjectFromFile[(Boolean,B)](tempFile)
    result
  }

  def main(args: Array[String]): Unit = {
    val (false, input) = FileInteraction.readObjectFromFile[(Boolean,A)](tempFile)
    val result = f(input)
    FileInteraction.saveObjectToFile(tempFile)((true, result))
    System.exit(0)
  }
}

object ReverseList extends RemoteFunc[List[Int], List[Int]] {
  def tempFile = "tempFile.serialized"

  def f(xs: List[Int]) = xs.reverse

  def jarPath: String = "target/scala-2.12/PatternSynthBenchmarkDriver.jar"
}

object TestRemoteFunc{
  def main(args: Array[String]): Unit = {
    println{
      ReverseList(List(1,2,3,4,5))
    }
  }
}