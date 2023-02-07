package Chainsaw

import java.io.File
import spinal.core.Bool

import scala.collection.mutable.ArrayBuffer
import arithmetic.bitheap._

package object arithmetic {

  type BitHeapHard = ArrayBuffer[ArrayBuffer[Bit[Bool]]]

  val compressorSolutionOutputDir = {
    val dir = new File("src/main/resources/compressorSolutions")
    dir.mkdirs()
    dir
  }

  val defaultCompressorPerfGraphPath = new File(
    "src/main/resources/compressorPerfs"
  )

  val cpaWidthMax = 96

}
