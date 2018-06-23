package singularity

import java.io._
import java.nio.file.{Files, Paths}

import singularity.EvolutionRepresentation.IndividualData

class FileLogger(fileName: String, printToConsole: Boolean, writer: FileWriter) {

  def println(obj: Any): Unit ={
    val s = obj.toString
    writer.write(s)
    writer.write("\n")
    writer.flush()
    if(printToConsole){
      System.out.println(s)
    }
  }

  def print(obj: Any): Unit ={
    val s = obj.toString
    writer.write(s)
    if(printToConsole){
      System.out.print(s)
    }
  }

  def printSection[A](name: String)(content: => A): A = {
    println(s"[$name]")
    val r = content
    println(s"[End of $name]\n")
    r
  }
}

object FileInteraction{

  def writeToFile(filePath: String, append: Boolean = false)(content: String): Unit = {
    import java.io._
    val fw = new FileWriter(filePath, append)
    try {
      fw.write(content)
    }finally{
      fw.close()
    }
  }

  def writeToBinaryFile(filePath: String)(content: Array[Byte]): Unit = {
    import java.io._
    val fw = new DataOutputStream(new FileOutputStream(filePath))
    try {
      fw.write(content, 0, content.length)
    }finally{
      fw.close()
    }
  }

  def deleteIfExist(filePath: String): Unit = {
    Files.deleteIfExists(Paths.get(filePath))
  }

  def runWithAFileLogger[T](fileName: String, printToConsole: Boolean = true)(f: FileLogger => T): T = {
    mkDirsAlongPath(fileName.split("/").init.mkString("/"))
    val writer = new FileWriter(fileName)
    val logger = new FileLogger(fileName, printToConsole, writer)
    try{
      f(logger)
    }finally {
      writer.flush()
      writer.close()
    }
  }

  def mkDirsAlongPath(path: String): Unit = {
    val parts = path.split("/").filterNot(_.isEmpty)
    require(parts.length>=1, s"invalid path format: '$path'")
    parts.tail.scanLeft(parts.head){ case (p0, part) => p0 + "/" + part }.foreach{ p =>
      val f = new File(p)
      if(!f.exists()){
        f.mkdir()
      }
    }
  }

  def saveObjectToFile(path: String)(obj: Serializable): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    try{
      oos.writeObject(obj)
    } finally {
      oos.close()
    }
  }

  def readObjectFromFile[T](path: String): T = {
    val ois = new ObjectInputStream(new FileInputStream(path))
    try{
      val obj = ois.readObject.asInstanceOf[T]
      obj
    } finally {
      ois.close()
    }
  }

  def getWorkingDir(ioId: Int): String = {
    val workingDirPrefix = s"workingDir$ioId"
    val workingDirPath = java.nio.file.Files.createTempDirectory(workingDirPrefix)
    workingDirPath.toFile.deleteOnExit()
    workingDirPath.toString
  }

  def saveMultiIndToFile(path: String)(ind: MultiStateInd): Unit = ind match {
    case MultiStateInd(exprs, nStates) =>
      saveObjectToFile(path)(exprs.map(SExpr.mkSExpr) -> nStates)
  }

  def readMultiIndFromFile(path: String, funcMap: Map[String, EFunction]): MultiStateInd = {
    val (sExprs, numOfStates) = readObjectFromFile[(IS[SExpr], Int)](path)
    MultiStateInd(sExprs.map(e => SExpr.mkExpr(e, funcMap)), numOfStates)
  }

}


