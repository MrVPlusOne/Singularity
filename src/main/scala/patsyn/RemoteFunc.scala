package patsyn

import patsyn.FileInteraction.ClassPath
import patsyn.RemoteFunc.{RemoteFailure, RemoteInput, RemoteResult, RemoteSuccess}

trait RemoteFunc[A <: Serializable, B <: Serializable] {
  def tempFileName = "tempFile.serialized"

  def f(input: A, workingDir: String): B

  def apply(input: A, workingDir: String)(implicit cp: ClassPath) = {
    val classPath = cp.path
    require(FileInteraction.isAbsolutePath(workingDir), "workingDir is required to be absolute path")
    require(FileInteraction.isAbsolutePath(classPath), "classPath is required to be absolute path")
    import java.nio.file._

    val tempFile = s"$workingDir/$tempFileName"
    FileInteraction.saveObjectToFile(tempFile)(RemoteInput(input))

    val javaPath = Paths.get(System.getProperty("java.home"), "bin", "java").toFile.getAbsolutePath
//    val jarPath = new File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath).getAbsolutePath

    val mainClassName = {
      val s = this.getClass.getName
      assert(s.last == '$')
      s.init
    }

    val cmd = s"$javaPath -cp $classPath $mainClassName $workingDir"

    import sys.process._
    Process(cmd.split("\\s+").toSeq, new java.io.File(workingDir)).!
    FileInteraction.readObjectFromFile[RemoteResult[B]](tempFile) match {
      case RemoteSuccess(result) => result
      case RemoteFailure(ex) =>
        System.err.println("Remote Exception: ")
        throw ex
    }
  }

  def main(args: Array[String]): Unit = {
    val workingDir = args.head
    val tempFile = s"$workingDir/$tempFileName"

    try {
      val input = FileInteraction.readObjectFromFile[RemoteInput[A]](tempFile).input
      val result = f(input, workingDir)
      FileInteraction.saveObjectToFile(tempFile)(RemoteSuccess(result))
    } catch {
      case ex: Exception => FileInteraction.saveObjectToFile(tempFile)(RemoteFailure(ex))
    } finally {
      System.exit(0)
    }
  }
}

object RemoteFunc{
  case class RemoteInput[A](input: A)

  trait RemoteResult[+A] extends Serializable
  case class RemoteSuccess[A](input: A) extends RemoteResult[A]
  case class RemoteFailure(msg: Exception) extends RemoteResult[Nothing]
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