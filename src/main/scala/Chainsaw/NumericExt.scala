package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

object NumericExt {

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