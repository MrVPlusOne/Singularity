package patsyn

import java.io.{File, FileWriter}

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

object FileLogger{
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
}


