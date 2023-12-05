package Chainsaw.arithmetic.flopoco

import Chainsaw.{ChainsawOperatorModule, NumericType, TestCase, pow2}
import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow.vivado._
import spinal.lib._
import spinal.core._
import spinal.lib.fsm._
import spinal.core.sim._

import scala.util.Random

/** Implements a DSP block commonly found in FPGAs incl. pre-adders and post-adders computing R = (X1+X2) * Y + Z
  *
  * @param wX
  *   width of X(include sign bit if X is signed)
  * @param wY
  *   width of Y(include sign bit if Y is signed)
  * @param wZ
  *   width of Z(include sign bit if X is signed)
  * @param xIsSigned
  *   whether X is signed
  * @param yIsSigned
  *   whether Y is signed
  * @param isPipelined
  *   whether insert register in each level
  * @param usePostAdder
  *   compute X * Y +Z
  * @param usePreAdder
  *   compute (X1 + X2) * Y
  * @param preAdderSubtracts
  *   compute (X1 - X2) * Y
  */
case class DSPBlock(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wX: Int,
    wY: Int,
    wZ: Int,
    xIsSigned: Boolean,
    yIsSigned: Boolean,
    isPipelined: Boolean,
    usePostAdder: Boolean,
    usePreAdder: Boolean,
    preAdderSubtracts: Boolean
) extends FlopocoOperator(family, targetFrequency) {

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "DSPBlock"
  override val entityName   = "DSPBlock"
  override val params = {
    if (usePostAdder)
      Seq(
        ("wX", wX),
        ("wY", wY),
        ("wZ", wZ),
        ("xIsSigned", if (xIsSigned) 1 else 0),
        ("yIsSigned", if (yIsSigned) 1 else 0),
        ("isPipelined", if (isPipelined) 1 else 0),
        ("usePostAdder", 1),
        ("usePreAdder", if (usePreAdder) 1 else 0),
        ("preAdderSubtracts", if (preAdderSubtracts) 1 else 0)
      )
    else
      Seq(
        ("wX", wX),
        ("wY", wY),
        ("xIsSigned", if (xIsSigned) 1 else 0),
        ("yIsSigned", if (yIsSigned) 1 else 0),
        ("isPipelined", if (isPipelined) 1 else 0),
        ("usePreAdder", if (usePreAdder) 1 else 0),
        ("preAdderSubtracts", if (preAdderSubtracts) 1 else 0)
      )
  }

  val widthOut = wX + wY
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X  = (!usePreAdder).generate(in Bits (wX bits))
      val X1 = usePreAdder.generate(in Bits (wX bits))
      val X2 = usePreAdder.generate(in Bits (wX bits))
      val Y  = in Bits (wY bits)
      val Z  = usePostAdder.generate(in Bits (wZ bits))
      val R  = out Bits (widthOut bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    if (!usePreAdder) {
      box.X                   := flowIn.fragment(0).asBits
      box.Y                   := flowIn.fragment(1).asBits
      if (usePostAdder) box.Z := flowIn.fragment(2).asBits
    } else {
      box.X1                  := flowIn.fragment(0).asBits
      box.X2                  := flowIn.fragment(1).asBits
      box.Y                   := flowIn.fragment(2).asBits
      if (usePostAdder) box.Z := flowIn.fragment(3).asBits
    }
    flowOut.fragment.head.assignFromBits(box.R)
  }

  override def implNaiveH = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = {
    if (usePreAdder && usePostAdder) {
      Seq(
        NumericType(wX - (if (xIsSigned) 1 else 0), 0, signed = xIsSigned),
        NumericType(wX - (if (xIsSigned) 1 else 0), 0, signed = xIsSigned),
        NumericType(wY - (if (yIsSigned) 1 else 0), 0, signed = yIsSigned),
        NumericType(wZ - (if (xIsSigned) 1 else 0), 0, signed = xIsSigned)
      )
    } else if (!usePreAdder && usePostAdder) {
      Seq(
        NumericType(wX - (if (xIsSigned) 1 else 0), 0, signed = xIsSigned),
        NumericType(wY - (if (yIsSigned) 1 else 0), 0, signed = yIsSigned),
        NumericType(wZ - (if (xIsSigned) 1 else 0), 0, signed = xIsSigned)
      )
    } else if (usePreAdder && !usePostAdder) {
      Seq(
        NumericType(wX - (if (xIsSigned) 1 else 0), 0, signed = xIsSigned),
        NumericType(wX - (if (xIsSigned) 1 else 0), 0, signed = xIsSigned),
        NumericType(wY - (if (yIsSigned) 1 else 0), 0, signed = yIsSigned)
      )
    } else {
      Seq(
        NumericType(wX - (if (xIsSigned) 1 else 0), 0, signed = xIsSigned),
        NumericType(wY - (if (yIsSigned) 1 else 0), 0, signed = yIsSigned)
      )
    }
  }

  override def outputTypes: Seq[NumericType] = {
    if (xIsSigned) Seq(NumericType.S(widthOut - 1))
    else Seq(NumericType.U(widthOut))
  }

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = {
    if (usePreAdder && preAdderSubtracts)
      Seq((testCase.data(0) - testCase.data(1)) * testCase.data(2) + (if (usePostAdder) testCase.data(3) else 0))
    else if (usePreAdder)
      Seq((testCase.data(0) + testCase.data(1)) * testCase.data(2) + (if (usePostAdder) testCase.data(3) else 0))
    else
      Seq(testCase.data(0) * testCase.data(1) + (if (usePostAdder) testCase.data(2) else 0))
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = {
    def inRange(width: Int, signed: Boolean, data: BigInt): Boolean = {
      val maxValue = if (signed) pow2(width - 1) - 1 else pow2(width) - 1
      val minValue = if (signed) -pow2(width - 1) else 0.toBigInt
      if ((data >= minValue) && (data <= maxValue)) true
      else false
    }

    def getVector: Seq[BigDecimal] = {
      var X1 = BigInt(wX, Random) - (if (xIsSigned) BigInt(1) << (wX - 1) else 0)
      var X2 = BigInt(wX, Random) - (if (xIsSigned) BigInt(1) << (wX - 1) else 0)
      var Y  = BigInt(wY, Random) - (if (yIsSigned) BigInt(1) << (wY - 1) else 0)
      var Z  = BigInt(wZ, Random) - (if (xIsSigned) BigInt(1) << (wZ - 1) else 0)

      def satisfied: Boolean = {
        val preAdderNotOverflow = {
          if (usePreAdder) inRange(wX, xIsSigned, if (preAdderSubtracts) X1 - X2 else X1 + X2)
          else true
        }

        val resultNotOverflow = {
          if (usePreAdder)
            inRange(
              widthOut,
              xIsSigned,
              (if (preAdderSubtracts) (X1 - X2) * Y else (X1 + X2) * Y) + (if (usePostAdder) Z else 0.toBigInt)
            )
          else inRange(widthOut, xIsSigned, X1 * Y + (if (usePostAdder) Z else 0.toBigInt))
        }

        preAdderNotOverflow && resultNotOverflow
      }

      while (!satisfied) {
        X1 = BigInt(wX, Random) - (if (xIsSigned) BigInt(1) << (wX - 1) else 0)
        X2 = BigInt(wX, Random) - (if (xIsSigned) BigInt(1) << (wX - 1) else 0)
        Y  = BigInt(wY, Random) - (if (yIsSigned) BigInt(1) << (wY - 1) else 0)
        Z  = BigInt(wZ, Random) - (if (xIsSigned) BigInt(1) << (wZ - 1) else 0)
      }

      if (usePreAdder && usePostAdder) Seq(X1, X2, Y, Z).map(BigDecimal(_))
      else if (!usePreAdder && usePostAdder) Seq(X1, Y, Z).map(BigDecimal(_))
      else if (usePreAdder && !usePostAdder) Seq(X1, X2, Y).map(BigDecimal(_))
      else Seq(X1, Y).map(BigDecimal(_))
    }
    Seq.fill(100)(TestCase(getVector))
  }
}
