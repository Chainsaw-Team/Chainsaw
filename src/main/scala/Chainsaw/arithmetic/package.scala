package Chainsaw

import Chainsaw.deprecated.{DagPort, Resize, Split, SplitN}

import java.io.File
import Chainsaw.deprecated._
import spinal.core.Bool

import scala.collection.mutable.ArrayBuffer
import arithmetic.bitheap._

package object arithmetic {

  type BitHeapHard = ArrayBuffer[ArrayBuffer[Bit[Bool]]]

  val compressorSolutionDir = {
    val dir = new File("src/main/resources/compressorSolutions")
    dir.mkdirs()
    dir
  }

  val defaultCompressorPerfGraphPath = new File("src/main/resources/compressorPerfs")

  val cpaWidthMax = 96

}
