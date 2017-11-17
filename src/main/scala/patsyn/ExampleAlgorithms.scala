package patsyn

import java.awt.image.BufferedImage

import edu.utexas.stac.Cost
import patsyn.GeneticOperator.ExprGen
import patsyn.StandardSystem.{EInt, EVect, IntComponents, IntValue, VectComponents, VectValue, _}

import scala.concurrent.TimeoutException
import scala.util.Random


trait FuzzingTaskProvider{
  protected def task: RunningFuzzingTask

  def sizeF: PartialFunction[IS[EValue], Int]

  def setupTask(task: RunningFuzzingTask): Unit = {}

  def teardownTask(task: RunningFuzzingTask): Unit = {}

  def displayValue: PartialFunction[IS[EValue], String] = {
    case v => v.toString
  }

  def saveValueWithName(value: IS[EValue], name: String): Unit = {
    import java.io._
    val fw = new FileWriter(new File(name+".txt"))
    try{
      fw.write(displayValue(value))
    } finally {
      fw.close()
    }
  }

  def run[A](f: RunningFuzzingTask => A): A = {
    val task = this.task
    try{
      setupTask(task)
      f(task)
    }finally{
      teardownTask(task)
    }
  }
}


case class RunningFuzzingTask(outputTypes: IS[EType],
                              sizeOfInterest: Int = 500,
                              resourceUsage: PartialFunction[IS[EValue], Double],
                              gpEnv: GPEnvironment
                      )

//noinspection TypeAnnotation
object FuzzingTaskProvider{

  def emptyAction(): Unit = ()

  def notPossible[T](): T = throw new Exception("Not possible!")

  def toIntVect(v: Vector[EValue]): Vector[Int] = {
    v.asInstanceOf[Vector[IntValue]].map(_.value)
  }

  def makeConstMap(pairs: (EType, IS[Random => EValue])*): Map[EType, IS[ExprGen[EConst]]] = {
    pairs.map{ case (t, fs) =>
      t -> fs.map(f =>ExprGen(t, r => EConst(t, f(r))))
    }.toMap
  }

  def sortingEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def insertionSortExample = new FuzzingTaskProvider {
    import patbench.slowfuzz.InsertionSort

    def sizeF = {
      case IS(VectValue(v)) => v.length
    }

    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      outputTypes = IS(EVect(EInt)),
      resourceUsage = {


        case IS(VectValue(v)) =>
          Cost.reset()
          InsertionSort.main(toIntVect(v).map(i=>i.toString).toArray)
          Cost.read()
      },
      gpEnv = sortingEnv
    )
  }

  def quickSortExample = new FuzzingTaskProvider {
    import patbench.slowfuzz.QuickSort

    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      outputTypes = IS(EVect(EInt)),
      resourceUsage = {
        case IS(VectValue(vec)) =>
          Cost.reset()
          val args = toIntVect(vec).map(i=>i.toString).toArray
          QuickSort.main(args)
          Cost.read()
      },
      gpEnv = sortingEnv
    )

    def sizeF = {
      case IS(VectValue(v)) =>
        v.length
    }
  }

  def vectIntToString(vec: VectValue): String = {
    String.valueOf(vectIntToCharArray(vec).toArray)
  }

  def vectIntToCharArray(vec: VectValue): List[Char] = {
    vec.value.map { i =>
      (i.asInstanceOf[IntValue].value % 256).toChar
    }.toList
  }

  def phpHashEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector()),
      EVect(EVect(EInt)) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def phpHashCollision = new FuzzingTaskProvider {
    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      outputTypes = IS(EVect(EVect(EInt))),
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val hashes = vec.map(v => {
            vectIntToCharArray(v.asInstanceOf[VectValue])
          }).distinct.map(ccs_hashFunc)

          hashes.groupBy(identity).values.map{
            elems => elems.length - 1
          }.sum
      },
      gpEnv = phpHashEnv
    )

    def sizeF = {
      case IS(VectValue(strings)) =>
        strings.map(s => s.asInstanceOf[VectValue].value.length).sum
    }
  }

  def toLowercase(i: Int): Char = {
    ('a' + i % 26).toChar
  }

  def abcRegexEnv: GPEnvironment = {
    val symbols = "abcABC ,.\\(){}[]+-*/=_".toCharArray.toIndexedSeq
    val intGens: IS[Random => IntValue] = IS(
      r => r.nextInt(7),
      r => SimpleMath.randomSelect(r)(symbols).toInt
    )

    val constMap = makeConstMap(
      EInt -> intGens,
      EVect(EInt) -> IS(_ => Vector()),
      //      EVect(EVect(EInt)) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def intValueAsChar(eValue: EValue): Char = {
    eValue.asInstanceOf[IntValue].value.toChar
  }

  def regexExample(regex: String, regexDic: Int => String) = new FuzzingTaskProvider {
    import patbench.slowfuzz.regex._
    val pattern = Pattern.compile(regex)


    override def displayValue = {
      case IS(VectValue(vs)) =>
      vs.map(intValueAsChar).mkString("")
    }

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = 200,
        resourceUsage = {
          case IS(VectValue(chars)) =>
            Cost.reset()
            val s = chars.asInstanceOf[Vector[IntValue]].flatMap(i => regexDic(i.value))
            pattern.matcher(s).find()
            Cost.read()
        },
        gpEnv = abcRegexEnv
      )
    }

    def sizeF = {
      case IS(VectValue(chars)) => chars.length
    }
  }

  def ccs_hashFunc(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = 5381
    for(i <- cs.indices){
      hash = ((hash << 5) + hash) + cs(i)
    }
    hash
  }

  def intVectorToGraph(graphId: Int, refs: Vector[Int]): String = {
    val graphName = if(graphId == 0) "main" else s"L$graphId"
    val graphBody = refs.zipWithIndex.map { case (ref, i) =>
      val edgeName = s"$graphName-$i"
      val label = if(ref == graphId) "net" else s"container:L$ref"
      s"""    $edgeName [type="$label", color="red", x=30, y=30, h=30, w=30];"""
    }.mkString("\n")
    s"""graph "$graphName" {
       |$graphBody
       |}
     """.stripMargin
  }

  def graphAnalyzerExample(workingDir: String) = new FuzzingTaskProvider {

    def sizeF = {
      case IS(VectValue(graphs)) =>
        graphs.map(g => g.asInstanceOf[VectValue].value.length+1).sum
    }

    protected def task: RunningFuzzingTask = {
      import java.io.FileWriter

      import edu.utexas.stac.Cost
      import patbench.graphanalyzer.user.commands.CommandProcessor

      RunningFuzzingTask(
        outputTypes = IS(EVect(EVect(EInt))),
        sizeOfInterest = 12,
        resourceUsage = {
          case IS(VectValue(graphs)) =>

            val fileContent = graphs.zipWithIndex.map{
              case (graphVec, i) =>
                val vec = graphVec.asInstanceOf[VectValue].value.map{e => e.asInstanceOf[IntValue].value}
                intVectorToGraph(i, vec)
            }.mkString("\n")

            val fw = new FileWriter(s"$workingDir/genGraph.dot")
            fw.write(fileContent)
            fw.close()

            Cost.reset()

            val timeLimit = 10*1000
            try {
              import scala.concurrent.ExecutionContext.Implicits.global
              import scala.concurrent._
              import scala.concurrent.duration._

              Await.result(Future(
                CommandProcessor.main(s"dot $workingDir/genGraph.dot xy diagram png output-files/PNG_output.png".split(" "))
              ), timeLimit.milliseconds)

              Cost.read().toDouble
            } catch {
              case _: NullPointerException =>
                Cost.read().toDouble
              case _: TimeoutException =>
                println("Timed out!")
                System.exit(1)
                throw new Exception("Timed out!")
            }
        },
        gpEnv = phpHashEnv
      )
    }
  }


  /** Http request example */
  def bloggerExample = new FuzzingTaskProvider {
    import patbench.blogger.fi.iki.elonen.JavaWebServer

    import sys.process._
    val server = new JavaWebServer(8080)

    override def setupTask(task: RunningFuzzingTask): Unit = {
      new Thread(() => server.start()).start()
    }

    override def teardownTask(task: RunningFuzzingTask): Unit = {
      server.stop()
    }


    override def displayValue = {
      case IS(vec:VectValue)=> vectIntToString(vec)
    }

    def sizeF = {
      case IS(VectValue(chars)) =>
        chars.length
    }

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = 100,
        resourceUsage = {
          case IS(vec :VectValue) =>
            val s = vectIntToString(vec)
            Cost.reset()
            try{
              s"curl -i http://localhost:8080/$s" !
            } catch {
              case _: java.io.IOException =>
            }
            Cost.read().toDouble
        },
        gpEnv = sortingEnv
      )
    }
  }

  def imageEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection ++ IS(IntComponents.shiftByteLeft)

    val stateTypes = constMap.keys.toIndexedSeq ++ IS(EInt, EInt)
    GPEnvironment(constMap, functions, stateTypes)
  }

  def intsToImage(imageWidth: Int, imageHeight: Int, name: String, data: IS[Int]): BufferedImage = {
    import java.awt.image.BufferedImage

    val imageDataSize = imageWidth * imageHeight
    val dataSize = data.length
    val content = (0 until imageDataSize).flatMap { i =>
      val intValue = data(SimpleMath.wrapInRange(i, dataSize))
      IS((intValue >> 24) & 255, intValue & 255, (intValue >> 8) & 255, (intValue >> 16) & 255)
    }
    val image = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_ARGB)
    image.getRaster.setPixels(0, 0, imageWidth, imageHeight, content.toArray)
    image
  }

  def parseCost(s: String): Long = {
    val Array(first, costS) = s.split("\\s+")
    assert(first == "[COST]")
    costS.toLong
  }

  def imageExample(imageWidth: Int, imageHeight: Int, workingDir: String) = new FuzzingTaskProvider {
    import java.io.File
    import javax.imageio.ImageIO

    import sys.process._

    def sizeF = {
      case IS(VectValue(data)) => data.length
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      val data = value.head
      val width = math.max(1, math.sqrt(data.size).toInt)
      val image = intsToImage(width, width, name,
        data.asInstanceOf[VectValue].value.map(_.asInstanceOf[IntValue].value))
      ImageIO.write(image, "png", new File(name + ".png"))
    }

    protected def task: RunningFuzzingTask = {
      var bestPerformanceSoFar = Double.MinValue

      val imageDataSize = imageWidth * imageHeight
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = imageDataSize,
        resourceUsage = {
          case IS(VectValue(data)) =>
            if(data.isEmpty) 0.0
            else {
              val imageName = s"$workingDir/genImage"
              val imagePath  = imageName + ".png"
              val image = intsToImage(imageWidth, imageHeight, imageName, data.map(_.asInstanceOf[IntValue].value))
              ImageIO.write(image, "png", new File(imagePath))

              val jarPath = "benchmarks/image_processor/challenge_program/ipchallenge-ins.jar"
              val results = Seq("java", "-Xint", "-jar", s"$jarPath", "cluster", imagePath).lineStream
              val cost = parseCost(results.last)
              val performance = cost.toDouble
              if (performance > bestPerformanceSoFar) {
                ImageIO.write(image, "png", new File(s"$workingDir/bestImageSoFar.png"))
                bestPerformanceSoFar = performance
              }
              performance
            }
        },
        gpEnv = imageEnv
      )
    }
  }

  def escapeStrings(s: String): String = {
    import org.apache.commons.lang3.StringEscapeUtils

    StringEscapeUtils.escapeJava(s)
  }

  def textCrunchrExample(workDir: String) = new FuzzingTaskProvider {
    import java.util.zip._
    import java.io._
    import sys.process._

    def outputAsZip(outPath: String, content: String, contentFileName: String = "content.txt") = {
      val f = new File(outPath)
      val zipStream = new ZipOutputStream(new FileOutputStream(f))
      val entry = new ZipEntry(contentFileName)
      zipStream.putNextEntry(entry)

      zipStream.write(content.getBytes)
      zipStream.close()
    }

    def sizeF: PartialFunction[IS[EValue], Int] = {
      case IS(VectValue(chars)) => chars.length
    }

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = 5000,
        resourceUsage = {
          case IS(chars: VectValue) =>
            val content = vectIntToString(chars)
            val zipPath = s"$workDir/input.zip"
            outputAsZip(zipPath, content)

            val cost = parseCost(Seq("java", "-Xint", "-cp", "benchmarks/textChrunchr/textChrunchr_3.jar",
              "com.cyberpointllc.stac.host.Main", zipPath).lineStream.last)

            println(s"Cost = $cost")
            cost.toDouble
        },
        gpEnv = abcRegexEnv.copy(
          stateTypes = abcRegexEnv.stateTypes ++ IS(EInt, EVect(EInt)),
          functions = IntComponents.collection ++ VectComponents.collection ++
            IS(VectComponents.shift, VectComponents.intToString)
        )
      )
    }


    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      value match {
        case IS(VectValue(chars)) =>
          val content = vectIntToString(chars)
          val zipPath = name+".zip"
          outputAsZip(zipPath, content)
      }
    }
  }

  def linearAlgebraEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EVect(EInt) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = IS(EInt, EInt, EInt, EVect(EInt), EVect(EInt))
    GPEnvironment(constMap, functions, stateTypes)
  }

  def linearAlgebraExample(matSize: Int, workingDir: String) = new FuzzingTaskProvider {
    val rowSize, midSize, colSize: Int = matSize

    import patbench.linearalgebra.com.example.linalg.external.serialization.OperationRequest
    import patbench.linearalgebra.com.example.linalg.external.serialization.OperationRequest$Argument

    override def displayValue = {
      case IS(lhs:VectValue, rhs:VectValue)=>
        s"(LHS = $lhs, RHS = $rhs)"
    }

    override def sizeF: PartialFunction[IS[EValue], Int] = {
      case IS(VectValue(lhs), VectValue(rhs)) => lhs.length + rhs.length
    }

    def toMatrixString(data: IS[Int], width: Int, height: Int): String = {
      val matrixSize = width * height
      val (dataSize, dataSeq) = if (data.isEmpty) { (1, IS(0)) } else { (data.length, data) }
      (0 until matrixSize)
        .map(i => math.abs(dataSeq(SimpleMath.wrapInRange(i, dataSize))))
        .map(i => "%.16f".format(i.toDouble / 1000))
        .grouped(width)
        .map(row => row.mkString(","))
        .mkString("\n")
    }

    def toRequest(lhs: String, lhsWidth: Int, lhsHeight: Int,
                  rhs: String, rhsWidth: Int, rhsHeight: Int): OperationRequest = {
      val req = new OperationRequest
      req.operation = 1
      req.numberOfArguments = 2

      val arg0 = new OperationRequest$Argument(req)
      arg0.rows = lhsWidth
      arg0.cols = lhsHeight
      arg0.matrix = lhs
      val arg1 = new OperationRequest$Argument(req)
      arg1.rows = rhsWidth
      arg1.cols = rhsHeight
      arg1.matrix = rhs
      req.args = Array(arg0, arg1)

      req
    }

    def makeRequestFromVectValues(lhs: VectValue, rhs: VectValue): OperationRequest = {
      val lhsMatrix = toMatrixString(toIntVect(lhs.value), rowSize, midSize)
      val rhsMatrix = toMatrixString(toIntVect(rhs.value), midSize, colSize)
      toRequest(
        lhsMatrix, rowSize, midSize,
        rhsMatrix, midSize, colSize)
    }

    override protected def task: RunningFuzzingTask = {
      import patbench.linearalgebra.com.example.linalg.external.operations.MultiplyOperation

      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt), EVect(EInt)),
        sizeOfInterest = rowSize * midSize + midSize * colSize,
        resourceUsage = {
          case IS(lhs: VectValue, rhs: VectValue) =>
            val req = makeRequestFromVectValues(lhs, rhs)
            import patbench.linearalgebra.com.google.gson.Gson
            println(new Gson().toJson(req))

            Cost.reset()
            new MultiplyOperation().compute(req)
            Cost.read()
        },
        gpEnv = linearAlgebraEnv
      )
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      import java.io.FileWriter
      import java.io.IOException
      import patbench.linearalgebra.com.google.gson.Gson

      super.saveValueWithName(value, name)
      value match {
        case IS(lhs: VectValue, rhs: VectValue) =>
          val jsonFileName = name + ".json"
          val req = makeRequestFromVectValues(lhs, rhs)

          try {
            val writer = new FileWriter(jsonFileName)
            try {
              val gson = new Gson
              gson.toJson(req, writer)
            } finally if (writer != null) writer.close()
          } catch {
            case e: IOException => println(s"Error when writing to file \'$jsonFileName\': ${e.getMessage}")
          }
      }
    }
  }

  def airplanEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> IS(r => r.nextInt(12)),
      EPair(EInt, EInt) -> IS(r => r.nextInt(12) -> r.nextInt(12)),
      EVect(EPair(EInt, EInt)) -> IS(_ => Vector()),
      EVect(EVect(EPair(EInt, EInt))) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection ++ PairComponents.collection

    val stateTypes = IS(EInt, EInt, EPair(EInt, EInt), EVect(EPair(EInt, EInt))) ++ constMap.keys
    GPEnvironment(constMap, functions, stateTypes)
  }

  abstract class AirplanFuzzingTaskProvider extends FuzzingTaskProvider {

    type WeightedGraph = IS[IS[(Int, Int)]]

    def valueToGraph(graphValue: IS[EValue]): WeightedGraph = {
      val numNodes = graphValue.length
      def toAdjList (srcIdx: Int, vec: IS[EValue]): IS[(Int, Int)] = {


        val edgeDict = vec.foldLeft(Map[Int, Int]())( (edgeDict, elemValue: EValue) => {
          val (dstValue, weightValue) = elemValue.asInstanceOf[PairValue].value
          val dst = Math.floorMod(dstValue.asInstanceOf[IntValue].value, numNodes)
          val weight = weightValue.asInstanceOf[IntValue].value
          if (srcIdx == dst) edgeDict else edgeDict + (dst -> weight)
        })
        edgeDict.toIndexedSeq
      }

      graphValue.zipWithIndex.map {
        case (l, srcIdx) => toAdjList(srcIdx, l.asInstanceOf[VectValue].value)
      }
    }

    def airportsToString(numAirports: Int): String = {
      val airportNames = (0 until numAirports).map(i => s"$i").mkString("\n")
      s"$numAirports\n$airportNames\n"
    }

    def writeRouteMapToFile(graph: WeightedGraph, fileName: String) = {
      FileInteraction.writeToFile(fileName)(routeMapToString(graph, fileName))
    }

    def prepareRouteMap(origin: Int, dest: Int, graph: WeightedGraph, routeMapFileName: String) = {
      val numNodes = graph.length
      val originName = if (numNodes == 0) "0" else s"${Math.floorMod(origin, numNodes)}"
      val destName = if (numNodes == 0) "0" else s"${Math.floorMod(dest, numNodes)}"

      writeRouteMapToFile(graph, routeMapFileName)
      (originName, destName)
    }

    override def sizeF: PartialFunction[IS[EValue], Int] = {
      case IS(IntValue(_), IntValue(_), VectValue(graph)) =>
        val numEdges = graph.map(g => g.asInstanceOf[VectValue].value.length).sum
        val numNodes = graph.size
        numNodes + numEdges
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)

      value match {
        case IS(_, _, VectValue(graph)) =>
          writeRouteMapToFile(valueToGraph(graph), s"$name.routemap.txt")
      }
    }

    def routeMapToString(graph: WeightedGraph, fileName: String): String
  }

  def airplan1Example(workingDir: String) = new AirplanFuzzingTaskProvider {

    def edgesToString(graph: WeightedGraph): String = {
      val numEdges = graph.map(l => l.length).sum
      val edgeLines = graph.zipWithIndex.map {
        case (adjList, srcIdx) =>
          adjList.map(edgePair =>
            s"$srcIdx ${edgePair._1} ${edgePair._2} 0 0 0 0 0"
          ).mkString("\n")
      }.mkString("\n")
      s"$numEdges\n$edgeLines"
    }

    override def routeMapToString(graph: WeightedGraph, fileName: String) = {
      airportsToString(graph.length) + edgesToString(graph)
    }

    override protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EInt, EInt, EVect(EVect(EPair(EInt, EInt)))),
        sizeOfInterest = 100,
        resourceUsage = {
          case IS(origin: IntValue, dest: IntValue, VectValue(graph)) =>
            import patbench.airplan1.edu.utexas.stac.AirplanNoServer

            val routeMapFileName = s"$workingDir/routemap.txt"
            val (originName, destName) = prepareRouteMap(
              origin.value,
              dest.value,
              valueToGraph(graph),
              routeMapFileName)

            Cost.reset()
            AirplanNoServer.run(workingDir, routeMapFileName, originName, destName)
            Cost.read()
        },
        gpEnv = airplanEnv
      )
    }
  }

  def airplan2Example(workingDir: String) = new AirplanFuzzingTaskProvider {

    def edgesToString(graph: WeightedGraph): String = {
      val numEdges = graph.map(l => l.length).sum
      val edgeLines = graph.zipWithIndex.map {
        case (adjList, srcIdx) =>
          adjList.map(edgePair =>
            s"$srcIdx ${edgePair._1} ${math.abs(edgePair._2)} 0 0 0 0 0"
          ).mkString("\n")
      }.mkString("\n")
      s"$numEdges\n$edgeLines"
    }

    override def routeMapToString(graph: WeightedGraph, fileName: String) = {
      airportsToString(graph.length) + edgesToString(graph)
    }

    override protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EInt, EInt, EVect(EVect(EPair(EInt, EInt)))),
        sizeOfInterest = 100,
        resourceUsage = {
          case IS(origin: IntValue, dest: IntValue, VectValue(graph)) =>
            import patbench.airplan2.edu.utexas.stac.AirplanNoServer

            val routeMapFileName = s"$workingDir/routemap.txt"
            val (originName, destName) = prepareRouteMap(
              origin.value,
              dest.value,
              valueToGraph(graph),
              routeMapFileName)

            Cost.reset()
            AirplanNoServer.run(workingDir, routeMapFileName, originName, destName)
            Cost.read()
        },
        gpEnv = airplanEnv
      )
    }
  }

  /*def sendCommand(cmd: String): Unit = {
    import sys.process._

    cmd.split("\\s+").toSeq.!
  }

  def gabFeed2Example(ioId: Int, workDir: String) = new FuzzingTaskProvider {
    import gabfeed2.ServerManager

    val port = 8080+ioId
    val serverMain = ServerManager.makeServer(port, "benchmarks/gabfeed2/data", false,
      "benchmarks/gabfeed2/ServersPrivateKey.txt","benchmarks/gabfeed2/ServersPasswordKey.txt", null)

    override def setupTask(task: RunningFuzzingTask): Unit = {
      serverMain.start()
    }

    override def teardownTask(task: RunningFuzzingTask): Unit = {
      serverMain.stop()
    }

    def sizeF: PartialFunction[IS[EValue], Int] = {
      case IS(VectValue(chars)) => chars.length
    }

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        outputTypes = IS(EVect(EInt)),
        sizeOfInterest = 300,
        resourceUsage = {

          case IS(chars:VectValue) =>
            val content = vectIntToString(chars)

            val messagePath = s"$workDir/gabfeed2.txt"
            FileInteraction.writeToFile(messagePath)(content)

            Cost.reset()
            sendCommand(s"curl -s -c cookies.txt -F username=foo -F password=df89gy9Qw --insecure https://localhost:$port/login")
            sendCommand(s"curl -s -F messageContents=@$messagePath -b cookies.txt --insecure https://localhost:$port/newmessage/1_0")
            sendCommand(s"curl -s -L -b cookies.txt --insecure https://localhost:$port/thread/1_0?suppressTimestamp=true")

            Cost.read().toDouble
        },
        gpEnv = abcRegexEnv.copy(stateTypes = abcRegexEnv.stateTypes ++ IS(EInt, EVect(EInt)))
      )
    }
  }*/

}
