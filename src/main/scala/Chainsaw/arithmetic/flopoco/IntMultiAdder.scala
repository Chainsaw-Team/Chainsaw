package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core.{IntToBuilder, _}
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

case class IntMultiAdder(widthIn: Int, n: Int, signed: Boolean)
    extends FlopocoOperator {

  /** -------- params for Flopoco generation
    * --------
    */
  override val operatorName = "IntMultiAdder"
  override val family       = UltraScale
  override val params =
    Seq(("signedIn", if (signed) 1 else 0), ("n", n), ("wIn", widthIn))

  /** black box used in synthesis
    */
  override def blackbox = new FlopocoBlackBoxWithClk {
    val X = in Vec (Bits(widthIn bits), n)
    X.zipWithIndex.foreach { case (int, i) => int.setName(s"X$i") }
    val R = out Bits (widthOut bits)

    override def mapChainsawModule(
        flowIn: Flow[Fragment[Vec[AFix]]],
        flowOut: Flow[Fragment[Vec[AFix]]]
    ): Unit = {
      X := flowIn.fragment.map(_.asBits)
      flowOut.fragment.head.assignFromBits(R)
    }
  }

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    dataOut.head := dataIn.reduce(_ + _).d().truncated
  })

  override def name =
    s"${operatorName}_w${widthIn}_n${n}_${if (signed) "signed" else "unsigned"}"

  override def vivadoUtilEstimation = VivadoUtil(dsp = 0, lut = n * widthIn)

  override def fmaxEstimation = 600 MHz

  override def inputTypes =
    if (!signed) Seq.fill(n)(NumericType.U(widthIn))
    else Seq.fill(n)(NumericType.S(widthIn - 1))

  val widthOut = widthIn + log2Up(n)

  override def outputTypes =
    if (!signed) Seq(NumericType.U(widthOut))
    else Seq(NumericType.S(widthOut - 1))

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase) = Seq(testCase.data.sum)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = {
    val getVector =
      if (!signed) Seq.fill(n)(BigInt(widthIn, Random)).map(BigDecimal(_))
      else
        Seq
          .fill(n)(BigInt(widthIn, Random) - (BigInt(1) << (widthIn - 1)))
          .map(BigDecimal(_))
    Seq.fill(100)(TestCase(getVector))
  }
}
