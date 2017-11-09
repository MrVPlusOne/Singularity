package patsyn

import java.io._

import patsyn.EvolutionRepresentation.IndividualData

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
}

object FileInteraction{
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

  def main(args: Array[String]): Unit = {
    val objFile = "results/Tue Oct 03 17:15:54 CDT 2017/bestIndividual[seed=2].serialized"

    val pattern = "abc".r
    pattern.regex

    val arg = readObjectFromFile[IndividualData[MultiStateInd]](objFile)
    println(arg.ind)
  }
}


