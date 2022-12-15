package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.arithmetic.flopoco.XilinxGpc
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim.SpinalSimBackendSel.GHDL

abstract class Gpc extends ChainsawOperatorGenerator with Compressor {

  def name = s"${className(this)}"

  override def simBackEnd = GHDL

  // column in
  override def inputTypes =
    inputFormat.filter(_ > 0).map(NumericType.U)

  // row out
  override def outputTypes = {
    require(outputFormat.max == 1)
    Seq(NumericType.U(outputFormat.length))
  }

  val inputWeights = inputFormat.zipWithIndex.filter(_._1 > 0).map(_._2)

  override def impl(testCase: TestCase) = {
    val ret = testCase.data.zip(inputWeights).map { case (bit, weight) =>
      bit.toBigInt().toString(2).count(_ == '1') << weight
    }.sum
    Seq(BigDecimal(ret))
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    yours.head.toBigInt() == golden.head.toBigInt()
  }

  override def testCases = Seq.fill(100)(randomTestCase)

  override def compress(bitsIn: BitHeapHard) = ???

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    val ret = dataIn.zip(inputWeights).map { case (bits, weight) =>
      bits.asBits.asBools.map(_.asUInt).reduce(_ +^ _) << weight
    }.reduce(_ +^ _)
    dataOut.head := ret.toAFix.truncated
  })

  // TODO: get rid of flopoco and VHDL(by using verilog blackbox / primitives)
  override def useNaive = if (!super.useNaive && testVhdl) false else true

  val flopocoGen = XilinxGpc(inputFormat.reverse, outputFormat.length)
  require(flopocoGen.latency() == 0)

  // TODO: get rid of flopoco and VHDL(by using verilog blackbox / primitives)
  override def implH = flopocoGen.implH
}

/** --------
 * from flopoco, clbCost = 0.5
 * -------- */
class HalfClbCompressor(override val inputFormat: Seq[Int]) extends Gpc {

  override def outputFormat = Seq.fill(5)(1)

  override def vivadoUtilEstimation = VivadoUtilEstimation(lut = 4, carry8 = 1) // clbCost = 0.5, can two of this share the same CARRY8?
}

object Compressor606 extends HalfClbCompressor(Seq(6, 0, 6).reverse)

object Compressor607 extends HalfClbCompressor(Seq(6, 0, 7).reverse)

object Compressor615 extends HalfClbCompressor(Seq(6, 1, 5).reverse)

object Compressor623 extends HalfClbCompressor(Seq(6, 2, 3).reverse)

object Compressor1325 extends HalfClbCompressor(Seq(1, 3, 2, 5).reverse)

object Compressor1415 extends HalfClbCompressor(Seq(1, 4, 1, 5).reverse)

object Compressor1406 extends HalfClbCompressor(Seq(1, 4, 0, 6).reverse)

object Compressor1407 extends HalfClbCompressor(Seq(1, 4, 0, 7).reverse)

object Compressor2117 extends HalfClbCompressor(Seq(2, 1, 1, 7).reverse)

// TODO: more GPC of other size