package patsyn

import patsyn.FileInteraction.ClassPath

trait RemoteFunc[A <: Serializable, B <: Serializable] {
  def tempFileName = "tempFile.serialized"

  def f(input: A, workingDir: String): B

  def apply(input: A, workingDir: String)(implicit cp: ClassPath) = {
    val classPath = cp.path
    require(FileInteraction.isAbsolutePath(workingDir), "workingDir is required to be absolute path")
    require(FileInteraction.isAbsolutePath(classPath), "classPath is required to be absolute path")
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

    val cmd = Debug.log("cmd")(s"$javaPath -cp $classPath $mainClassName $workingDir")

    import sys.process._
    Process(cmd.split("\\s+").toSeq, new java.io.File(workingDir)).!
    val (true, result) = FileInteraction.readObjectFromFile[(Boolean,B)](tempFile)
    result
  }

  def main(args: Array[String]): Unit = {
    val workingDir = args.head
    val tempFile = s"$workingDir/$tempFileName"
    val (false, input) = FileInteraction.readObjectFromFile[(Boolean,A)](tempFile)
    val result = f(input, workingDir)
    FileInteraction.saveObjectToFile(tempFile)((true, result))
    System.exit(0)
  }
}

object ReverseList extends RemoteFunc[List[Int], List[Int]] {

  def f(xs: List[Int], workingDir: String) = xs.reverse
}

object TestRemoteFunc{
  def main(args: Array[String]): Unit = {
    val workingDir = FileInteraction.getCurrentWorkingDir()
    implicit val classPath: FileInteraction.ClassPath = FileInteraction.getClassPath(inIDE = true)

    println{
      ReverseList(List(1,2,3,4,5), workingDir)
    }
  }
}