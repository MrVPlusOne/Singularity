package benchmarks

import java.util.Base64

import com.stac.database._
import com.stac.encoder._
import patsyn.{Debug, Playground, SimpleMath}

object StacSQLTest {

  def treeVector(tree: HuffmanNode): Vector[(Byte, Int)] = {
    if(tree == null) return Vector()

    treeVector(tree.getLeft) ++ Vector(tree.getValue.toByte -> tree.getCodeLength) ++ treeVector(tree.getRight)
  }

  lazy val fibs: Stream[Int] = Stream.cons(1,Stream.cons(1,fibs.zip(fibs.tail).map{
    case (a,b) => a + b
  }))

  def testHuffman(): Unit ={

    val data1: Array[Byte] = fibs.take(15).zipWithIndex.flatMap {
      case (n, i) => Vector.fill(n)(i.toByte)
    }.toArray
    println("data1 length: " + data1.length)
    val tree = HuffmanCoder.constructTree(data1)
    println{
      treeVector(tree)
    }
  }

  def printTreeFreq(tree: HuffmanNode): Unit ={
    println{
      treeVector(tree).sortBy(_._2).reverse
    }
  }

  def theory2(): Unit ={
    val data1 = Array[Byte](0,1,2)
    val tree0 = HuffmanCoder.constructTree(data1)
    printTreeFreq(tree0)

    var tree1 = tree0
    for(i <- 0 until 1000){
      tree1 = HuffmanCoder.updateTree(Array[Byte]((3+i).toByte), tree1)
    }

    printTreeFreq(tree1)
  }

  def theory2_impl(): Unit ={
    val data1 = Array[Byte](0,1,2)

    val field1 = new SQLField("filed1", SQLDataTypes.BLOB, data1)
    Debug.log("data1 length"){
      field1.getRawData.length
    }

    for(i <- 0 until 254){
      field1.setData(Array[Byte]((3+i).toByte))
    }

    field1.setData((0 until 100).map(_ => 0.toByte).toArray)

    Debug.log("data2 length"){
      field1.getRawData.length
    }
  }

  def encode64(data: Array[Byte]): String = {
    String.valueOf(Base64.getEncoder.encode(data).map(_.toChar))
  }

  def encodeWithAs(i: Int): String = {
    val s = Playground.compressNumToString(i)
    "a"*(4-s.length)+s
  }

  lazy val validEncodes = Stream.from(0,1).map{ i => encode64(Array.fill(4)(i.toByte))}.filterNot(_.contains("="))

  def printScript(): Unit = {
    val rowNum = 130
//    val codes = validEncodes.take(rowNum).toVector

    val initData = encode64(Array(0,1,2))

    println("create table t (d blob)")
    for(i <- 0 until rowNum){
      println(s"insert into t values ($initData)")
    }

    val updateNum = 138+20
    for(i <- 0 until updateNum){
      val data = encode64(Array(0,0,(i+3).toByte))
      println(s"update t set d = $data")
    }

    val largeData = Array.fill(245)(Array[Byte](0,1,2)).flatten
    println(s"update t set d = ${encode64(largeData)}")
  }

  def main(args: Array[String]): Unit = {
    printScript()



//    val enco = Base64.getEncoder.encode(Array[Byte](1,3,4,5,17))
//
//    Debug.log("encoded length")(enco.length)
//    Debug.log("decoded length"){
//      Base64.getDecoder().decode(enco).length
//    }
    return

    theory2_impl()

//    val distribution = (1 to 50)
    val distribution = fibs.take(17)


    val data1: Array[Byte] = distribution.zipWithIndex.flatMap {
      case (n, i) => Vector.fill(n)(i.toByte)
    }.toArray
    println("data1 length: " + data1.length)
    val tree = HuffmanCoder.constructTree(data1)
    println{
      treeVector(tree).toList.sortBy(_._2).reverse
    }

    val dataSize = 1000
    val data2: Array[Byte] = (0 until dataSize).map(_ => 1.toByte).toArray

    println{
      "Encoded length: " + HuffmanCoder.encodeBytes(data2, tree).length
    }

    val field1 = new SQLField("filed1", SQLDataTypes.INT, data1)
    Debug.log("data1 length"){
      field1.getRawData.length
    }
    field1.setData(data2)
    Debug.log("data2 length"){
      field1.getRawData.length
    }
  }
}
