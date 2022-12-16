package Chainsaw

import Chainsaw.deprecated.NumericType
import breeze.math.Complex
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

object NumericExt {

  // extension for signed fix
  implicit class SFixUtil(sf: SFix) {

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

  implicit class afixUtil(afix: AFix) {
    def numericType = new NumericType(afix.maxRaw, afix.minRaw, afix.exp)

    // FIXME: fix the negate() method in SpinalHDL
    def symmetricNegate = {
      //      val ret = new AFix(-afix.minRaw max afix.maxRaw, -afix.maxRaw min afix.minRaw, afix.exp)
      val ret = new AFix(afix.maxRaw, afix.minRaw, afix.exp)
      if (afix.signed) ret.raw := (-afix.raw.asSInt).asBits
      else ret.raw := U(afix.raw).twoComplement(True, null).asBits
      ret
    }

    /** scaling the range to [-1,1)
     */
    def normalized = afix >> (if (afix.signed) afix.intWidth - 1 else afix.intWidth)

    // FIXME: precise range will be lost
    //    def fixTo(that: NumericType) = afix.fixTo(that.qFormat)
  }

  implicit class hardVecUtil(vec: Seq[AFix]) {

    def toComplexFix = vec.grouped(2).toSeq.map { case Seq(a, b) => ComplexFix(a, b) }

    def pipelinedBalancedTree(op: (AFix, AFix) => AFix, latency: Int) = {
      def pipeline(op: AFix, i: Int) = op.d(latency)

      vec.reduceBalancedTree(op, pipeline)
    }

    def fixTo(af:AFix) = vec.map(_.fixTo(af))

    def normalized = vec.map(_.normalized)
  }


  implicit class softVecUtil(vec: Seq[BigDecimal]) {
    def toComplex = vec.map(_.toDouble).grouped(2).toSeq.map { case Seq(a, b) => Complex(a, b) }
  }

}