package Chainsaw

import java.io.File

package object arithmetic {

  val compressorSolutionDir = {
    val dir =  new File("src/main/resources/compressorSolutions")
    dir.mkdirs()
    dir
  }

  val defaultCompressorPerfGraphPath = new File("src/main/resources/compressorPerfs")

  val ternaryWidthMax = 96
  val binaryWidthMax = 96

  def getBinaryWidths(fullWidth:Int) = {
    val n = fullWidth.divideAndCeil(binaryWidthMax)
    (fullWidth - (n- 1) * binaryWidthMax) +: Seq.fill(n - 1)(binaryWidthMax)
  }
}
