package Chainsaw

import spinal.core._

import java.io.File
import scala.language.postfixOps

package object deprecated {

  sealed trait Direction

  object In extends Direction

  object Out extends Direction

  implicit class ChainsawGeneratorUtil(gen: ChainsawGenerator) {

    // by this, we don't need to implement asVertex inside ChainsawGenerator, avoiding two-way dependence
    def asVertex(implicit dag: Dag) = DagVertex(gen)

  }

  val dagFigDir = {
    val dir = new File("src/main/resources/dfgGenerated")
    dir.mkdirs()
    dir
  }

  val cpaWidthMax = 64

  def getBinaryWidths(fullWidth: Int) = {
    val n = fullWidth.divideAndCeil(cpaWidthMax)
    (fullWidth - (n - 1) * cpaWidthMax) +: Seq.fill(n - 1)(cpaWidthMax)
  }

  implicit class karatsubaPortUtil(port: DagPort)(implicit dag: Dag) {

    def splitAt(lowWidth: Int) = {
      val s = Split(port.width, lowWidth).asVertex
      s := port
      (s.out(0), s.out(1))
    }

    def takeLow(width: Int) = port.splitAt(width)._2

    def takeHigh(width: Int) = port.splitAt(port.width - width)._1

    def splitN(n: Int) = {
      val s = SplitN(port.width, n).asVertex
      s := port
      s.outPorts
    }

    def +^(that: DagPort) = {
      val add = CpaS2S(BinaryAdder, that.width max port.width, withCarry = true).asVertex
      add := (port, that)
      add.out(0)
    }

    def resize(widthOut: Int) = {
      val re = Resize(port.width, widthOut).asVertex
      re := port
      re.out(0)
    }

    def <<(shiftLeft: Int) = {
      val shift = ShiftLeft(shiftLeft, port.width).asVertex
      shift := port
      shift.out(0)
    }
  }

  implicit class UIntUtil(ui: UInt) {
    def numericType = UIntInfo(ui.getWidth)
  }

  implicit class SIntUtil(si: UInt) {
    def numericType = SIntInfo(si.getWidth - 1)
  }

  // extension for signed fix
  implicit class SFixUtil(sf: SFix) {

    def numericType = SFixInfo(sf.maxExp, -sf.minExp)

    def isPositive: Bool = ~sf.raw.msb

    def isNegative: Bool = ~isPositive

    def abs: SFix = {
      val ret = cloneOf(sf)
      ret.raw := sf.raw.abs.asSInt
      ret
    }

    def unary_-(): SFix = {
      val ret = SFix(sf.maxExp exp, sf.minExp exp)
      ret.raw := -sf.raw
      ret
    }

    private def doAddSub(that: SFix, add: Boolean): SFix = {
      val (rawLeft, rawRight) = sf.alignLsb(that)
      val ret = SFix(Math.max(sf.maxExp, that.maxExp) + 1 exp, Math.max(rawLeft.getBitsWidth, rawRight.getBitsWidth) + 1 bits)
      ret.raw := (if (add) rawLeft +^ rawRight else rawLeft -^ rawRight)
      ret
    }

    def +^(that: SFix): SFix = doAddSub(that, add = true)

    def -^(that: SFix): SFix = doAddSub(that, add = false)

    def truncate(dataType: HardType[SFix]): SFix = {
      val ret = dataType()
      ret := sf.truncated
      ret
    }

    def fractionalPart = {
      val ret = SFix(0 exp, sf.minExp exp)
      ret.assignFromBits(sf.asBits.msb ## sf.asBits.takeLow(-sf.minExp))
      ret
    }
  }

  implicit class BitsUtil(bits: Bits) {
    def toSFix(numericType: NumericType) = {
      val ret = numericType.asSFix()
      ret.assignFromBits(bits)
      ret
    }

    def toComplexFix(numericType: NumericType) = {
      val ret = numericType.asComplexFix()
      ret.assignFromBits(bits)
      ret
    }

    def withImag(imag: Bits, numericType: NumericType) = {
      val sf = numericType.toSFixInfo
      ComplexFix(bits.toSFix(sf), imag.toSFix(sf))
    }
  }

}
