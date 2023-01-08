package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** n-split Karatsuba, rectangular multiplier is supported
  *
  * @param width
  *   width of base multiplier
  * @param height
  *   height of base multiplier
  * @param split
  *   number of splits
  */
case class BaseMultiplierDSPKaratsuba(width: Int, height: Int, split: Int)
    extends FlopocoOperator {

  val n = split - 1

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    dataOut.head := dataIn.reduce(_ * _).d(latency())
  })

  override def vivadoUtilEstimation = VivadoUtil()

  // TODO: width inference for rectangular multiplier
  val widthX = width * split
  val widthY = height * split
  val widthR = widthX + widthY

  override def inputTypes = Seq(NumericType.U(widthX), NumericType.U(widthY))

  override def outputTypes = Seq(NumericType.U(widthR))

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase) = Seq(testCase.data.product)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = {
    val getVector = Seq(BigInt(widthX), BigInt(widthY)).map(BigDecimal(_))
    Seq.fill(1000)(TestCase(getVector))
  }

  /** -------- params for Flopoco generation
    * --------
    */
  override def name = s"${operatorName}_w${width}_h${height}_n$n"

  override val operatorName = "BaseMultiplierDSPKaratsuba"
  override val entityName   = "IntKaratsuba"
  override val family       = UltraScale

  override def fmaxEstimation = 600 MHz

  override val params = Seq(("wX", width), ("wY", height), ("n", n))

  /** black box used in synthesis
    */
  override def blackbox = new FlopocoBlackBoxWithClk {
    val X = in Bits (widthX bits)
    val Y = in Bits (widthY bits)
    val R = out Bits (widthR bits)

    override def mapChainsawModule(
        flowIn: Flow[Fragment[Vec[AFix]]],
        flowOut: Flow[Fragment[Vec[AFix]]]
    ): Unit = {
      X                  := flowIn.payload(0).asBits
      Y                  := flowIn.payload(1).asBits
      flowOut.payload(0) := R.asUInt.toAFix
    }
  }
}
