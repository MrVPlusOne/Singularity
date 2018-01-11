package benchmarks

import javax.crypto.{KeyGenerator, SecretKey}

import com.ainfosec.ibasys.client.IBASysClient

object IBASys {
  def main(args: Array[String]): Unit = {
    attackingClient()
  }

  def attackingClient(): Unit ={
    IBASysClient.runMaliciousClient(Array("client", "localhost", "hansolo", "images/correct_image.jpg"))
  }

  def emptyByteSeq(blockSize: Int, blocks: Int): Array[Byte] = {
    (0 until blocks).flatMap{ b =>
      (b until b+blockSize/4).map(_.toByte) ++ Array.fill(blockSize/4*3)(0.toByte)
    }.toArray
  }

  def getImageData(size: Int): Array[Byte] = {
    Array.fill(size)(1.toByte)
  }

  def getExtraData(blockSize: Int, repeats: Int): Array[Array[Byte]] = {
    (0 until repeats*2).map{ i =>
      if(i % 2 == 0){
        Array.fill(blockSize)(0.toByte)
      }else{
        Array.fill(blockSize/2)(1.toByte) ++ Array.fill(blockSize - blockSize/2)(0.toByte)
      }
    }.toArray
  }

  /** Question 24 success chance */
  def successRate(bytesToGuess: Int, budgetLeft: Int, byteSize: Int = 256): Double = {
    val buffer: Array[Array[Option[Double]]] = Array.fill(bytesToGuess+1)(Array.fill(budgetLeft+1)(None))

    def rec(bytesToGuess: Int, budgetLeft: Int): Double = {
      def buffered(p: Double) = {
        buffer(bytesToGuess)(budgetLeft) = Some(p)
        p
      }

      if(budgetLeft < 0 || budgetLeft == 0 && bytesToGuess > 0) return 0.0
      if(bytesToGuess == 0) return 1.0

      val b = buffer(bytesToGuess)(budgetLeft)
      if(b.nonEmpty)
        return b.get

      val maxGuess = math.min(budgetLeft, byteSize)
      val result = (0 until maxGuess).map{ guessNum =>
        rec(bytesToGuess - 1, budgetLeft - guessNum)
      }.sum / byteSize
      buffered(result)
    }

    rec(bytesToGuess, budgetLeft)
  }

  def checkSucessRate(): Unit ={
    println{
      successRate(16, 1) == math.pow(256, -16)
    }

    println{
      successRate(16, 2) > 2 * math.pow(256, -16)
    }

    println{
      successRate(16, 255*16) == 1.0
    }

    println{
      successRate(16, 2530)
    }
  }

}
