package Chainsaw.dsp

import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.xilinx.VivadoUtil
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/** fully-pipelined complex number multiplication using 3 multipliers
  *
  * input order = (real0, imag0, real1, imag1); output order = (real, imag)
  * @param dataType
  *   numeric type of the multiplier
  * @param coeffType
  *   numeric type of the multiplicand
  */
case class ComplexMult(dataType: NumericType, coeffType: NumericType)
    extends ChainsawOperatorGenerator
    with FixedLatency {

  override def name = s"ComplexMult_${dataType}_$coeffType"

  val retType = (dataType * coeffType).withCarry(1)

  override def inputTypes = Seq(dataType, dataType, coeffType, coeffType)

  override def outputTypes = Seq(retType, retType)

  override def vivadoUtilEstimation = VivadoUtil(dsp = 3, lut = 10)

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawOperatorModule(this) {
    // using SInt in tht datapath for better DSP inference, as - method of AFix lead to indirect RTL representation
    private val Seq(ar, ai, br, bi) = dataIn.map(_.raw.asSInt)
    // regs outside dsp
    private val arD1 = ar.d()
    private val aiD2 = ai.d(2)
    private val brD2 = br.d(2)
    private val biD2 = bi.d(2)
    // dsp operation and regs inside dsp
    private val mid = ((br.d() +^ bi.d()).d() * ar.d(2)).d(2)
    private val productImag =
      (mid.d() + ((aiD2.d() -^ arD1.d(2)).d() * brD2.d(2)).d()).d()
    private val productReal =
      (mid.d() - ((aiD2.d() +^ arD1.d(2)).d() * biD2.d(2)).d()).d()
    dataOut := Seq(productReal, productImag).map { sint =>
      val ret = retType()
      ret := sint
      ret
    }
  }

  override def implNaiveH = None

  override def latency() = 6

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase) = {
    val Seq(ar, ai, br, bi) = testCase.data
    Seq(ar * br - ai * bi, ar * bi + ai * br)
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.zip(golden).forall { case (y, g) => retType.same(y, g, 1e-2, 1e-2) }

  override def testCases = Seq.fill(100)(TestCase(randomDataVector))
}
