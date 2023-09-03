package Chainsaw

import java.io.File
import spinal.core.Bool

import scala.collection.mutable.ArrayBuffer
import arithmetic.bitheap._
import spinal.lib.experimental.math._
import spinal.core.sim._

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

  implicit class FloatingPointPimper(float: Floating) {
    def #=(value: Float) = {
      val bitValue = java.lang.Float.floatToIntBits(value)
      float.sign #= (bitValue >> 31) == 1
      float.exponent #= (bitValue >> 23) & 0xff
      float.mantissa #= bitValue & 0x7fffff
    }

    def toFloat = {
      val bitValue = (float.sign.toBigInt << 31) + (float.exponent.toBigInt << 23) + float.mantissa.toBigInt
      java.lang.Float.intBitsToFloat(bitValue.toInt)
    }
  }

}
