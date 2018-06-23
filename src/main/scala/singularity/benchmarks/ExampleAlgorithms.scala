package singularity.benchmarks

import java.awt.image.BufferedImage
import edu.utexas.stac.Cost
import singularity._
import GeneticOperator.ExprGen
import StandardSystem._
import scala.util.Random


//noinspection TypeAnnotation
object ExampleAlgorithms {

  def toIntVect(v: Vector[EValue]): Vector[Int] = {
    v.asInstanceOf[Vector[IntValue]].map(_.value)
  }

  def makeConstMap(pairs: (EType, Random => EValue)*): Map[EType, ExprGen[EConst]] = {
    pairs.map { case (t, f) =>
      t -> ExprGen(t, r => EConst(t, f(r)))
    }.toMap
  }

  def sortingEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> (r => r.nextInt(12)),
      EVect(EInt) -> (_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def insertionSortExample = new FuzzingTaskProvider {

    def outputTypes = IS(EVect(EInt))

    def sizeF = {
      case IS(VectValue(v)) => v.length
    }

    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      resourceUsage = {
        case IS(VectValue(v)) =>
          val intArray = toIntVect(v).toArray

          Cost.reset()
          patbench.slowfuzz.InsertionSort.sortPublic(intArray)
          Cost.read()
      },
      gpEnv = sortingEnv
    )
  }

  def quickSortExample = new FuzzingTaskProvider {

    def outputTypes = IS(EVect(EInt))

    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      resourceUsage = {
        case IS(VectValue(v)) =>
          val intArray = toIntVect(v).toArray
          Cost.reset()
          patbench.slowfuzz.QuickSort.sortPublic(intArray)
          Cost.read()
      },
      gpEnv = sortingEnv
    )

    def sizeF = {
      case IS(VectValue(v)) =>
        v.length
    }
  }

  def quickSortMiddlePivotExample = new FuzzingTaskProvider {

    def outputTypes = IS(EVect(EInt))

    protected def task: RunningFuzzingTask = RunningFuzzingTask(

      resourceUsage = {
        case IS(VectValue(v)) =>
          val intArray = toIntVect(v).toArray
          Cost.reset()
          patbench.slowfuzz.QuickSort.sortMiddlePivotPublic(intArray)
          Cost.read()
      },
      gpEnv = sortingEnv
    )

    def sizeF = {
      case IS(VectValue(v)) =>
        v.length
    }
  }

  def vectIntToString(vec: VectValue, charSize: Int = 16): String = {
    String.valueOf(vectIntToCharArray(vec, charSize).toArray)
  }

  def vectIntToCharArray(vec: VectValue, charSize: Int): List[Char] = {
    val range = 1 << charSize
    vec.value.map { i =>
      SimpleMath.wrapInRange(i.asInstanceOf[IntValue].value, range).toChar
    }.toList
  }

  def hashEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> (r => r.nextInt(12)),
      EVect(EInt) -> (_ => Vector()),
      EVect(EVect(EInt)) -> (_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes ++ stateTypes)
  }

  def hashCollisionExample(hashFunc: Seq[Char] => Int, charSize: Int) = new FuzzingTaskProvider {
    def totalCollisionMetric(vec: Vector[EValue]): Double = {
      val hashes = vec.map(v => {
        vectIntToCharArray(v.asInstanceOf[VectValue], charSize)
      }).distinct.map(hashFunc)

      hashes.groupBy(identity).values.map {
        elems => elems.length - 1
      }.sum
    }

    def squareMetric(vec: Vector[EValue]): Double = {
      val hashes = vec.map(v => {
        vectIntToCharArray(v.asInstanceOf[VectValue], charSize).takeWhile(ch => ch.toInt != 0)
      }).distinct.map(hashFunc)

      hashes.groupBy(identity).values.map{g =>
        val c = g.length - 1
        c*c
      }.sum
    }

    def outputTypes = IS(EVect(EVect(EInt)))


    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      resourceUsage = {
        case IS(VectValue(vec)) =>
          squareMetric(vec)
      },
      gpEnv = hashEnv
    )

    def sizeF = {
      case IS(VectValue(strings)) =>
        strings.map(s => s.asInstanceOf[VectValue].value.length + 1).sum
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      println(s"# of string = ${value(0).asInstanceOf[VectValue].value.length}")
    }
  }

  def phpHashCollisionExample = hashCollisionExample(HashFunc.php, 8)

  def javaHashCollisionExample = hashCollisionExample(HashFunc.java, 16)

  def rubyHashCollisionExample = hashCollisionExample(HashFunc.ruby, 8)

  def aspDotNetHashCollisionExample = hashCollisionExample(HashFunc.aspDotNet, 16)

  def pythonHashCollisionExample = hashCollisionExample(HashFunc.python, 8)

  def v8HashCollisionExample = hashCollisionExample(HashFunc.v8, 8)

  def murmur2HashCollisionExample = hashCollisionExample(HashFunc.murmur2(0), 8)


  private def hashPerformanceExample(hashFunc: Array[String] => Any, charSize: Int) = new FuzzingTaskProvider {
    def outputTypes = IS(EVect(EVect(EInt)))


    protected def task: RunningFuzzingTask = RunningFuzzingTask(
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val strs = vec.map(v => vectIntToString(v.asInstanceOf[VectValue], charSize)).filter(s => s.nonEmpty).toArray

          Cost.reset()
          hashFunc(strs)
          Cost.read()
      },
      gpEnv = hashEnv
    )

    def sizeF = {
      case IS(VectValue(strings)) =>
        strings.map(s => s.asInstanceOf[VectValue].value.length + 1).sum
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      println(s"# of string = ${value(0).asInstanceOf[VectValue].value.length}")
    }
  }

  def phpHashPerformanceExample = hashPerformanceExample(patbench.hash.edu.utexas.stac.HashHarness.phpStringHarness, 8)

  def javaHashPerformanceExample = hashPerformanceExample(patbench.hash.edu.utexas.stac.HashHarness
    .javaStringHarness, 16)

  def rubyHashPerformanceExample = hashPerformanceExample(patbench.hash.edu.utexas.stac.HashHarness
    .rubyStringHarness, 8)

  def aspDotNetHashPerformanceExample = hashPerformanceExample(patbench.hash.edu.utexas.stac.HashHarness
    .aspDotNetStringHarness, 16)

  def pythonHashPerformanceExample = hashPerformanceExample(patbench.hash.edu.utexas.stac.HashHarness
    .pythonStringHarness, 8)

  def v8HashPerformanceExample = hashPerformanceExample(patbench.hash.edu.utexas.stac.HashHarness.v8StringHarness, 8)

  def murmur2HashPerformanceExample = hashPerformanceExample(arr =>
    patbench.hash.edu.utexas.stac.HashHarness.murmur2StringHarness(arr, 0),
    8
  )

  def abcRegexEnv: GPEnvironment = {
    val symbols = "abcABC ,.\\(){}[]+-*/=_".toCharArray.toIndexedSeq

    def intGen(r: Random): IntValue = {
      if (r.nextDouble() < 0.5) r.nextInt(7)
      else SimpleMath.randomSelect(r)(symbols).toInt
    }

    val constMap = makeConstMap(
      EInt -> intGen,
      EVect(EInt) -> (_ => Vector()),
      //      EVect(EVect(EInt)) -> IS(_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def asciiRegexEnv: GPEnvironment = {
    val symbols = 1 until 128

    def intGen(r: Random): IntValue = {
      IntValue(SimpleMath.randomSelect(r)(symbols))
    }

    val constMap = makeConstMap(
      EInt -> intGen,
      EVect(EInt) -> (_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  def intValueAsChar(eValue: EValue): Char = {
    eValue.asInstanceOf[IntValue].value.toChar
  }

  def defaultRegexDic(x: Int): String = (x % 255).toChar.toString

  def regexExample(regex: String, regexDic: Int => String) = new FuzzingTaskProvider {

    import patbench.slowfuzz.regex._

    val pattern = Pattern.compile(regex)


    override def displayValue = {
      case IS(VectValue(vs)) =>
        vs.map(intValueAsChar).mkString("")
    }

    def outputTypes = IS(EVect(EInt))

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
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

  val SLOWFUZZ_REGEX1 = "(?i:(j|(&#x?0*((74)|(4A)|(106)|(6A));?))\n([\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n" +
    "(tab;)|(newline;))))*(a|(&#x?0*((65)|\n(41)|(97)|(61));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))\n))*(v|(&#x?0*((86)|(56)|(118)|(76));?)\n)([\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n(tab;)|(newline;))))*(a|(&#x?0*((65)|\n(41)|(97)|(61));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))))*\n(s|(&#x?0*((83)|(53)|(115)|(73));?))(\n[\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n(tab;)|(newline;))))*(c|(&#x?0*((67)|\n(43)|(99)|(63));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))))*\n(r|(&#x?0*((82)|(52)|(114)|(72));?))\n([\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n(tab;)|(newline;))))*(i|(&#x?0*((73)|\n(49)|(105)|(69));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))))*\n(p|(&#x?0*((80)|(50)|(112)|(70));?))\n([\\t]|(&((#x?0*(9|(13)|(10)|A|D);?)|\n(tab;)|(newline;))))*(t|(&#x?0*((84)|\n(54)|(116)|(74));?))([\\t]|(&((#x?0*(9|\n(13)|(10)|A|D);?)|(tab;)|(newline;))))\n*(:|(&((#x?0*((58)|(3A));?)|(colon;)\n))).)"
  val SLOWFUZZ_REGEX2 = "<(a|abbr|acronym|address|applet|area|\naudioscope|b|base|basefront|bdo|\nbgsound|big" +
    "|blackface|blink|\nblockquote|body|bq|br|button|caption|\ncenter|cite|code|col|colgroup|comment|dd|del|dfn|dir|div|dl|\ndt|em|embed|fieldset|fn|font|\nform|frame|frameset|h1|head|hr|\nhtml|i|iframe|ilayer|img|input|ins|\nisindex|kdb|keygen|label|layer|\nlegend|li|limittext|link|listing|\nmap|marquee|menu|meta|multicol|\nnobr|noembed|noframes|noscript|\nnosmartquotes|object|ol|optgroup|\noption|p|param|plaintext|pre|q|\nrt|ruby|s|samp|script|select|\nserver|shadow|sidebar|small|\nspacer|span|strike|strong|style|\nsub|sup|table|tbody|td|textarea|\ntfoot|th|thead|title|tr|tt|u|ul|\nvar|wbr|xml|xmp)\\\\W"
  val SLOWFUZZ_REGEX3 = "(?i:<.*[:]vmlframe.*?[ /+\\t]*?src[\n/+\\t]*=)"

  def slowFuzzRegexExample1 = regexExample(SLOWFUZZ_REGEX1, defaultRegexDic)

  def slowFuzzRegexExample2 = regexExample(SLOWFUZZ_REGEX2, defaultRegexDic)

  def slowFuzzRegexExample3 = regexExample(SLOWFUZZ_REGEX3, defaultRegexDic)

  def intVectorToGraph(graphId: Int, refs: Vector[Int]): String = {
    val graphName = if (graphId == 0) "main" else s"L$graphId"
    val graphBody = refs.zipWithIndex.map { case (ref, i) =>
      val edgeName = s"$graphName-$i"
      val label = if (ref == graphId) "net" else s"container:L$ref"
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
        graphs.map(g => g.asInstanceOf[VectValue].value.length + 1).sum
    }

    def outputTypes = IS(EVect(EVect(EInt)))


    protected def task: RunningFuzzingTask = {
      import edu.utexas.stac.Cost
      import patbench.graphanalyzer.user.commands.CommandProcessor


      RunningFuzzingTask(
        sizeOfInterest = 12,
        resourceUsage = {
          case IS(VectValue(graphs)) =>

            val fileContent = graphs.zipWithIndex.map {
              case (graphVec, i) =>
                val vec = graphVec.asInstanceOf[VectValue].value.map { e => e.asInstanceOf[IntValue].value }
                intVectorToGraph(i, vec)
            }.mkString("\n")

            FileInteraction.writeToFile(s"$workingDir/genGraph.dot")(fileContent)

            Cost.reset()

            try {
              CommandProcessor.main(s"dot $workingDir/genGraph.dot xy diagram png output-files/PNG_output.png".split(" "))

              Cost.read().toDouble
            } catch {
              case _: NullPointerException =>
                Cost.read().toDouble
            }
        },
        gpEnv = hashEnv
      )
    }
  }


  /** Http request example */
  def bloggerExample(ioId: Int) = new FuzzingTaskProvider {

    import patbench.blogger.fi.iki.elonen.JavaWebServer

    import sys.process._

    val server = new JavaWebServer(8080 + ioId)

    override def setupTask(task: RunningFuzzingTask): Unit = {
      new Thread(() => server.start()).start()
    }

    override def teardownTask(task: RunningFuzzingTask): Unit = {
      server.stop()
    }


    override def displayValue = {
      case IS(vec: VectValue) => vectIntToString(vec)
    }

    def sizeF = {
      case IS(VectValue(chars)) =>
        chars.length
    }

    def outputTypes = IS(EVect(EInt))

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        sizeOfInterest = 100,
        resourceUsage = {
          case IS(vec: VectValue) =>
            val s = vectIntToString(vec)
            Cost.reset()
            try {
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
      EInt -> (r => r.nextInt(12)),
      EVect(EInt) -> (_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection ++ IS(BitComponents.shiftByteLeft)

    val stateTypes = constMap.keys.toIndexedSeq ++ IS(EInt, EInt)
    GPEnvironment(constMap, functions, stateTypes)
  }

  def intsToImage(imageWidth: Int, imageHeight: Int, data: IS[Int]): BufferedImage = {
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

    def sizeF = {
      case IS(VectValue(data)) => data.length
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      val data = value.head
      val width = math.max(1, math.sqrt(data.memoryUsage).toInt)
      val image = intsToImage(width, width,
        data.asInstanceOf[VectValue].value.map(_.asInstanceOf[IntValue].value))
      ImageIO.write(image, "png", new File(name + ".png"))
    }

    def outputTypes = IS(EVect(EInt))

    protected def task: RunningFuzzingTask = {
      var bestPerformanceSoFar = Double.MinValue

      val imageDataSize = imageWidth * imageHeight
      RunningFuzzingTask(
        sizeOfInterest = imageDataSize,
        resourceUsage = {
          case IS(VectValue(data)) =>
            if (data.isEmpty) 0.0
            else {
              val image = intsToImage(imageWidth, imageHeight, data.map(_.asInstanceOf[IntValue].value))

              import patbench.imageprocessor.edu.utexas.stac.ImageProcessorNoReadFile
              Cost.reset()
              ImageProcessorNoReadFile.cluster(workingDir, image)
              Cost.read()
            }
        },
        gpEnv = imageEnv
      )
    }
  }

  def textCrunchrExample(workDir: String) = new FuzzingTaskProvider {

    import java.io._
    import java.util.zip._

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

    def outputTypes = IS(EVect(EInt))

    protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        sizeOfInterest = 5000,
        resourceUsage = {
          case IS(chars: VectValue) =>
            val content = vectIntToString(chars)
            val zipPath = s"$workDir/input.zip"
            outputAsZip(zipPath, content)

            Cost.reset()
            patbench.textcrunchr3.com.cyberpointllc.stac.host.Main.main(Array(zipPath))
            Cost.read()
        },
        gpEnv = abcRegexEnv.copy(
          stateTypes = abcRegexEnv.stateTypes ++ IS(EInt, EVect(EInt)),
          functions = IntComponents.collection ++ VectComponents.collection ++ AdvancedVectComponents.collection
        )
      )
    }


    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)
      value match {
        case IS(VectValue(chars)) =>
          val content = vectIntToString(chars)
          val zipPath = name + ".zip"
          outputAsZip(zipPath, content)
      }
    }
  }

  def linearAlgebraEnv: GPEnvironment = {
    val constMap = makeConstMap(
      EInt -> (r => r.nextInt(12)),
      EVect(EInt) -> (_ => Vector())
    )

    val functions = IntComponents.collection ++ VectComponents.collection

    val stateTypes = IS(EInt, EInt, EInt, EVect(EInt), EVect(EInt))
    GPEnvironment(constMap, functions, stateTypes)
  }

  def linearAlgebraExample(matSize: Int, workingDir: String) = new FuzzingTaskProvider {
    val rowSize, midSize, colSize: Int = matSize

    import patbench.linearalgebra.com.example.linalg.external.serialization.{OperationRequest, OperationRequest$Argument}

    override def displayValue = {
      case IS(lhs: VectValue, rhs: VectValue) =>
        s"(LHS = $lhs, RHS = $rhs)"
    }

    override def sizeF: PartialFunction[IS[EValue], Int] = {
      case IS(VectValue(lhs), VectValue(rhs)) => lhs.length + rhs.length
    }

    def toMatrixString(data: IS[Int], width: Int, height: Int): String = {
      val matrixSize = width * height
      val (dataSize, dataSeq) = if (data.isEmpty) {
        (1, IS(0))
      } else {
        (data.length, data)
      }
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

    def outputTypes = IS(EVect(EInt), EVect(EInt))

    override protected def task: RunningFuzzingTask = {
      import patbench.linearalgebra.com.example.linalg.external.operations.MultiplyOperation

      RunningFuzzingTask(
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
      import java.io.{FileWriter, IOException}

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
      EInt -> (r => r.nextInt(12)),
      EVect(EInt) -> (r => Vector()),
      EGraph(EInt) -> (r => GraphValue.empty),
    )

    val functions = IntComponents.collection ++ VectComponents.collection ++ GraphComponents.collection ++
      IS(GraphComponents.updateEdgeValue)

    val stateTypes = IS(EInt, EInt) ++ constMap.keys.toIndexedSeq
    GPEnvironment(constMap, functions, stateTypes)
  }

  abstract class MaxFlowFuzzingTaskProvider extends FuzzingTaskProvider {
    def sizeF = {
      case IS(IntValue(_), IntValue(_), graph: GraphValue) =>
        graph.memoryUsage.toInt
    }

    def graphToString(src: Int, snk: Int, graphValue: GraphValue): String = {
      val numNodes = graphValue.nodeNum
      val numEdges = graphValue.edges.length
      val edgeLines = graphValue.edges.map {
        case (src: Int, dst: Int, capacityValue: EValue) =>
          s"$src $dst ${capacityValue.asInstanceOf[IntValue].value}"
      }.mkString("\n")
      val srcInt = Math.floorMod(src, numNodes)
      val snkInt = Math.floorMod(snk, numNodes)
      s"$numNodes $numEdges\n$srcInt $snkInt\n$edgeLines\n"
    }

    def writeGraphToFile(src: Int, snk: Int, graphValue: GraphValue, fileName: String): Unit = {
      FileInteraction.writeToFile(fileName)(graphToString(src, snk, graphValue))
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)

      value match {
        case IS(IntValue(src), IntValue(snk), graph: GraphValue) =>
          writeGraphToFile(src, snk, graph, s"$name.graph.txt")
      }
    }
  }

  abstract class AirplanFuzzingTaskProvider extends FuzzingTaskProvider {

    def outputTypes = IS(EInt, EInt, EGraph(EInt))


    def airportsToString(numAirports: Int): String = {
      val airportNames = (0 until numAirports).map(i => s"$i").mkString("\n")
      s"$numAirports\n$airportNames\n"
    }

    def writeRouteMapToFile(graphValue: GraphValue, fileName: String) = {
      FileInteraction.writeToFile(fileName)(routeMapToString(graphValue, fileName))
    }

    def prepareRouteMap(origin: Int, dest: Int, graphValue: GraphValue, routeMapFileName: String) = {
      val GraphValue(numNodes, _) = graphValue
      val originName = if (numNodes == 0) "0" else s"${Math.floorMod(origin, numNodes)}"
      val destName = if (numNodes == 0) "0" else s"${Math.floorMod(dest, numNodes)}"

      writeRouteMapToFile(graphValue, routeMapFileName)
      (originName, destName)
    }

    override def sizeF: PartialFunction[IS[EValue], Int] = {
      case IS(IntValue(_), IntValue(_), graph: GraphValue) =>
        graph.memoryUsage.toInt
    }

    override def saveValueWithName(value: IS[EValue], name: String): Unit = {
      super.saveValueWithName(value, name)

      value match {
        case IS(_, _, graph: GraphValue) =>
          writeRouteMapToFile(graph, s"$name.routemap.txt")
      }
    }

    def routeMapToString(graph: GraphValue, fileName: String): String
  }

  def airplan1Example(workingDir: String) = new AirplanFuzzingTaskProvider {

    def edgesToString(edgeList: IS[(Int, Int, EValue)]): String = {
      val numEdges = edgeList.size
      val edgeLines = edgeList.map {
        case (src, dst, weightValue) =>
          s"$src $dst ${weightValue.asInstanceOf[IntValue].value} 0 0 0 0 0"
      }.mkString("\n")
      s"$numEdges\n$edgeLines"
    }

    override def routeMapToString(graphValue: GraphValue, fileName: String) = {
      airportsToString(graphValue.nodeNum) + edgesToString(graphValue.edges)
    }

    override protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        sizeOfInterest = 100,
        resourceUsage = {
          case IS(origin: IntValue, dest: IntValue, graphValue: GraphValue) =>
            import patbench.airplan1.edu.utexas.stac.AirplanNoServer

            if (graphValue.nodeNum == 0 || graphValue.edges.isEmpty)
              0
            else {
              val dbFileName = s"$workingDir/airplan.db"
              FileInteraction.deleteIfExist(dbFileName)

              val routeMapFileName = s"$workingDir/routemap.txt"
              val (originName, destName) = prepareRouteMap(
                origin.value,
                dest.value,
                graphValue,
                routeMapFileName)

              Cost.reset()
              AirplanNoServer.run(workingDir, routeMapFileName, originName, destName)
              Cost.read()
            }
        },
        gpEnv = airplanEnv
      )
    }
  }

  def airplan2Example(workingDir: String) = new AirplanFuzzingTaskProvider {

    def edgesToString(edgeList: IS[(Int, Int, EValue)]): String = {
      val numEdges = edgeList.size
      val edgeLines = edgeList.map {
        case (src, dst, weightValue) =>
          s"$src $dst ${weightValue.asInstanceOf[IntValue].value} 0 0 0 0 0"
      }.mkString("\n")
      s"$numEdges\n$edgeLines"
    }

    override def routeMapToString(graphValue: GraphValue, fileName: String) = {
      airportsToString(graphValue.nodeNum) + edgesToString(graphValue.edges)
    }

    override protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        sizeOfInterest = 100,
        resourceUsage = {
          case IS(origin: IntValue, dest: IntValue, graphValue: GraphValue) =>
            import patbench.airplan2.edu.utexas.stac.AirplanNoServer

            val dbFileName = s"$workingDir/airplan.db"
            FileInteraction.deleteIfExist(dbFileName)

            val routeMapFileName = s"$workingDir/routemap.txt"
            val (originName, destName) = prepareRouteMap(
              origin.value,
              dest.value,
              graphValue,
              routeMapFileName)

            Cost.reset()
            AirplanNoServer.run(workingDir, routeMapFileName, originName, destName)
            Cost.read()
        },
        gpEnv = airplanEnv
      )
    }
  }

  def airplan3Example(workingDir: String) = new AirplanFuzzingTaskProvider {

    def edgesToString(edgeList: IS[(Int, Int, EValue)]): String = {
      val numEdges = edgeList.size
      val edgeLines = edgeList.map {
        case (src, dst, weightValue) =>
          s"$src $dst ${math.abs(weightValue.asInstanceOf[IntValue].value)} 0 0 0 0 0"
      }.mkString("\n")
      s"$numEdges\n$edgeLines"
    }

    override def routeMapToString(graphValue: GraphValue, fileName: String) = {
      airportsToString(graphValue.nodeNum) + edgesToString(graphValue.edges)
    }

    override protected def task: RunningFuzzingTask = {
      RunningFuzzingTask(
        sizeOfInterest = 30,
        resourceUsage = {
          case IS(origin: IntValue, dest: IntValue, graphValue: GraphValue) =>
            import patbench.airplan3.edu.utexas.stac.AirplanNoServer

            val dbFileName = s"$workingDir/airplan.db"
            FileInteraction.deleteIfExist(dbFileName)

            val routeMapFileName = s"$workingDir/routemap.txt"
            val (originName, destName) = prepareRouteMap(
              origin.value,
              dest.value,
              graphValue,
              routeMapFileName)

            Cost.reset()
            AirplanNoServer.run(workingDir, routeMapFileName, originName, destName)
            Cost.read()
        },
        gpEnv = airplanEnv
      )
    }
  }

  object NativeSortExamples{
    def sortNativeExample(name: String)(workingDir: String) = new FuzzingTaskProvider {
      val native = new NativeExample(name, workingDir)
      import native._

      def sizeF = {
        case IS(VectValue(v)) => v.length
      }

      def outputTypes = IS(EVect(EInt))

      protected def task: RunningFuzzingTask = RunningFuzzingTask(
        sizeOfInterest = 64,
        resourceUsage = {
          case IS(VectValue(v)) =>
            val data = toIntVect(v).map(x => x.toByte).toArray
            writeByteArrayRunNativeGetCost(data)
        },
        gpEnv = sortingEnv
      )

      override def saveValueWithName(value: IS[EValue], name: String): Unit = {
        value match {
          case IS(VectValue(v)) =>
            val data = toIntVect(v).map(x => x.toByte).toArray
            val fileName = s"$name.bin"
            FileInteraction.writeToBinaryFile(fileName)(data)
        }
      }
    }

    def insertionSortNativeExample = sortNativeExample("isort") _

    def appleQsortNativeExample = sortNativeExample("appleqsort") _

    def bsdQsortNativeExample = sortNativeExample("bsdqsort") _

    def gnuQsortNativeExample = sortNativeExample("gnuqsort") _

    def pgQsortNativeExample = sortNativeExample("pgqsort") _

    def slowfuzzQsortNativeExample = sortNativeExample("qsort") _

    def phpHashNativeExample(inputSize: Int)(workingDir: String) = new FuzzingTaskProvider {
      val native = new NativeExample("phphash", workingDir)
      import native._

      def sizeF = {
        case IS(VectValue(v)) => v.length
      }

      def outputTypes = IS(EVect(EInt))

      protected def task: RunningFuzzingTask = RunningFuzzingTask(
        sizeOfInterest = inputSize, // 64 insertions and char_size is 2
        resourceUsage = {
          case IS(VectValue(v)) =>
            val data = toIntVect(v).map(x => x.toByte).toArray
            writeByteArrayRunNativeGetCost(data)
        },
        gpEnv = sortingEnv
      )

      override def saveValueWithName(value: IS[EValue], name: String): Unit = {
        value match {
          case IS(VectValue(v)) =>
            val data = toIntVect(v).map(x => x.toByte).toArray
            val fileName = s"$name.bin"
            FileInteraction.writeToBinaryFile(fileName)(data)
        }
      }
    }

    def regexNativeExample(regexId: Int, inputSize: Int)(workingDir: String) = new FuzzingTaskProvider {
      val native = new NativeExample("pcre_str", workingDir)
      import native._

      def sizeF = {
        case IS(VectValue(v)) => v.length
      }

      def outputTypes = IS(EVect(EInt))

      def writeByteArrayRunNativeGetCost(data: Array[Byte]): Double = {
        withWriteByteArray(data, "input", (inputFileName: String) => {
          val cmdParams = s"$inputFileName $regexId"
          runNativeGetCost(cmdParams)
        })
      }

      protected def task: RunningFuzzingTask = RunningFuzzingTask(
        sizeOfInterest = inputSize,
        resourceUsage = {
          case IS(VectValue(v)) =>
            val data = toIntVect(v).map(x => x.toByte).toArray
            writeByteArrayRunNativeGetCost(data)
        },
        gpEnv = sortingEnv
      )


      override def saveValueWithName(value: IS[EValue], name: String): Unit = {
        value match {
          case IS(VectValue(v)) =>
            val data = toIntVect(v).map(x => x.toByte).toArray
            val fileName = s"$name.bin"
            FileInteraction.writeToBinaryFile(fileName)(data)
        }
      }
    }

    //fixme: not working
    def bzipExample(workingDir: String) = new FuzzingTaskProvider {
      val native = new NativeExample("bzip", workingDir)
      import native._

      protected def task = {
        RunningFuzzingTask(
          sizeOfInterest = 250,
          resourceUsage = {
            case IS(VectValue(vec)) =>
              val data = toIntVect(vec).map(x => x.toByte).toArray
              writeByteArrayRunNativeGetCost(data)
          },
          gpEnv = {
            val constMap = makeConstMap(
              EInt -> (r => r.nextInt(255)),
              EInt -> (r => r.nextInt(255)),
              EInt -> (r => r.nextInt(255)),
              EVect(EInt) -> (_ => Vector()),
              EVect(EInt) -> (_ => Vector())
            )
            val functions = IntComponents.collection ++ BitComponents.collection ++ VectComponents.collection ++ AdvancedVectComponents.collection
            val stateTypes = constMap.keys.toIndexedSeq
            GPEnvironment(constMap, functions, stateTypes)
          }
        )
      }

      def outputTypes = IS(EVect(EInt))

      def sizeF = {
        case IS(vec) => vec.memoryUsage.toInt
      }
    }
  }

  class NativeExample(name: String, workingDir: String) {

    // We need this to be lazy since we don't want to actively looking for the binary if we do not need to run it
    lazy val nativeBinaryPath: String = {
      val arch = System.getProperty("os.arch") match {
        case "x86_64" | "amd64" => "amd64"
        case _@a => throw new RuntimeException(s"We currently do not prepare native binaries for architecture \'$a\'")
      }
      val os = System.getProperty("os.name") match {
        case "Mac OS X" => "macos"
        case "Linux" => "linux"
        case _@o => throw new RuntimeException(s"We currently do not prepare native binaries for OS \'$o\'")
      }
      val binPath = s"benchmarks/native/${arch}_${os}/$name"
      ensureExecutable(binPath)
      binPath
    }

    def ensureExecutable(filePath: String): Unit = {
      val file = new java.io.File(filePath)
      if (!file.exists)
        throw new RuntimeException(s"native binary file \'$filePath\' does not exist")
      if (!file.isFile)
        throw new RuntimeException(s"\'$filePath\' is not a file")
      if (!file.canExecute)
        throw new RuntimeException(s"\'$filePath\' is not executable")
    }

    def runNativeGetCost(cmdParams: String): Double = {
      import sys.process._

      val cmd = s"$nativeBinaryPath $cmdParams"
      try {
        val results = cmd.split("\\s+").toSeq.lineStream
        val cost = parseCost(results.last)
        cost.toDouble
      } catch {
        case _: RuntimeException =>
          // Program crashes are not interesting to us
          0
      }
    }

    def withWriteByteArray[T](data: Array[Byte], fileName: String, action: String => T): T = {
      val inputFileName = s"$workingDir/$fileName"
      FileInteraction.deleteIfExist(inputFileName)
      FileInteraction.writeToBinaryFile(inputFileName)(data)
      action(inputFileName)
    }

    def writeByteArrayRunNativeGetCost(data: Array[Byte]): Double = {
      withWriteByteArray(data, "input", runNativeGetCost)
    }
  }



  def bzipProblem(workingDir: String) = {
    val native = new NativeExample("bzip", workingDir)
    import native._

    ProblemConfig(
      "bzip",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(vec) => vec.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val data = toIntVect(vec).map(x => x.toByte).toArray
          writeByteArrayRunNativeGetCost(data)
      }
    )
  }
}
