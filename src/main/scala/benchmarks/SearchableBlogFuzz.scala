package benchmarks

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, RealMatrix, RealVector}
import patsyn._
import StandardSystem._
import com.bbn.MatrixRoutines

object SearchableBlogFuzz {
  val bVector: ArrayRealVector = {
    val line = io.Source.fromFile("matrix-data/b.txt").getLines().next()
    new ArrayRealVector(line.split(";\\s*").map(_.toDouble))
  }

  val ajMatrix: RealMatrix = {
    val data = io.Source.fromFile("matrix-data/A.txt").getLines().map{ line =>
      line.split(Array('{','}',',')).filter(_.nonEmpty).map(_.toDouble)
    }.toArray

    MatrixRoutines.processMatrix(new Array2DRowRealMatrix(data), 0.999)
  }

  def estimateStationaryVector(M: RealMatrix, v: RealVector, precision: Double): Long = {
    var count = 0L
//    val m = M.getRowDimension
    var b = v.unitVector
    var c = M.operate(b).unitVector
    while ( {
      b.getNorm > 1.0E-8 && Math.abs(b.getDistance(c) / b.getNorm) > precision
    }) {
      println(c.toString)
      b = c
      c = M.operate(b).unitVector
      count += 1
    }
    count
  }

  def searchableBlogConfig = {
    def normalizeVec(vec: VectValue): Array[Double] = {
      if(vec.value.isEmpty)
        return Array()
      val oneHotVec = vec.value.map{ e =>
        if(e.asInstanceOf[IntValue].value % 2 == 1) 1.0 else 0.0
      }.toArray
      (0 until bVector.getDimension).map{ i =>
        oneHotVec(SimpleMath.wrapInRange(i, oneHotVec.length))
      }.toArray
    }

    ProblemConfig(
      problemName = "searchableBlog.estimateStationaryVector",
      outputTypes = IS(EVect(EInt)),
      resourceUsage = {
        case IS(vec: VectValue) =>
          val nv = normalizeVec(vec)
          if(nv.sum > 8.0) 0
          else {
            val v1: RealVector = new ArrayRealVector(nv)
            val v = MatrixRoutines.processVector(v1, 0.999)
            val A = MatrixRoutines.concatenateColumn(ajMatrix, v)
            estimateStationaryVector(A, bVector, 1.0E-7).toDouble
          }
      },
      sizeF = {
        case IS(VectValue(vec)) => vec.length
      },
      saveValueWithName = (value, name) =>{
        FuzzingTaskProvider.defaultSaveValueWithName(
          value => normalizeVec(value.head.asInstanceOf[VectValue]).toVector.toString)(value, name)
      }
    )
  }

  def run(seed: Int, useGUI: Boolean): Unit = {
    import util.Random
    import Runner.RunnerConfig

    val rand = new Random(seed)
    Supernova.standard.fuzzProblem(
      searchableBlogConfig,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(bVector.getDimension), timeLimitInMillis = 100000), rand)
  }

  def main(args: Array[String]): Unit = {
    val input = FileInteraction.readObjectFromFile[Vector[VectValue]]("results-running/searchableBlog.estimateStationaryVector[ioId=3,seed=3](18-01-21-16:52:53)/bestInput.serialized")
    searchableBlogConfig.resourceUsage(input)
//    run(3,useGUI = true)
  }
}













