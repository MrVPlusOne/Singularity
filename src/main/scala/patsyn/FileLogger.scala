package patsyn

import java.io.FileWriter

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
    val writer = new FileWriter(fileName)
    val logger = new FileLogger(fileName, printToConsole, writer)
    try{
      f(logger)
    }finally {
      writer.flush()
      writer.close()
    }
  }
}


