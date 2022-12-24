package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.arithmetic.flopoco.{FlopocoBlackBox, XilinxGpc}
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim.SpinalSimBackendSel.GHDL
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

abstract class Gpc extends CompressorGenerator {

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
    val ret = testCase.data
      .zip(inputWeights)
      .map { case (bit, weight) => bit.toBigInt().toString(2).count(_ == '1') << weight }
      .sum
    Seq(BigDecimal(ret))
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    yours.head.toBigInt() == golden.head.toBigInt()
  }

  override def testCases = Seq.fill(100)(randomTestCase)

  override def compress(bitsIn: BitHeapHard): BitHeapHard = {
    val paddedBitsIn = bitsIn.zip(inputFormat).map { case (bits, h) => bits.padTo(h, False) }
    // in GPCs, different from row adders, we use columns as operands
    val operands = paddedBitsIn.map(_.asBits().asUInt.toAFix)
    val core     = getImplH
    core.dataIn := operands
    operands2Columns(core.dataOut, outputFormat).asInstanceOf[BitHeapHard]
  }

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    val ret = dataIn
      .zip(inputWeights)
      .map { case (bits, weight) =>
        bits.asBits.asBools.map(_.asUInt).reduce(_ +^ _) << weight
      }
      .reduce(_ +^ _)
    dataOut.head := ret.toAFix.truncated
  })

  // TODO: get rid of flopoco and VHDL(by using verilog blackbox / primitives)
  //     remove the flag testVhdl
  override def useNaive = if (!super.useNaive && (testVhdl || !atSimTime)) false else true

}

/** -------- from flopoco, clbCost = 0.5 --------
  */
class HalfClbGpc(override val inputFormat: Seq[Int]) extends Gpc {

  override def outputFormat = Seq.fill(5)(1)

  override def vivadoUtilEstimation = VivadoUtilEstimation(lut = 4, carry8 = 1, ff = 5) // clbCost = 0.5, can two of this share the same CARRY8?

  // TODO: implement blackbox
  override def implH: ChainsawOperatorModule = ???
}

class PrimitiveGpc(override val inputFormat: Seq[Int], override val outputFormat: Seq[Int], primitive: GpcPrimitive, utilEstimation: VivadoUtil) extends Gpc {

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    dataOut := primitive.primitiveCompress(dataIn)
  }

  override def vivadoUtilEstimation: VivadoUtil = utilEstimation

}

object Compressor606 extends HalfClbGpc(Seq(6, 0, 6).reverse)

object Compressor607 extends HalfClbGpc(Seq(6, 0, 7).reverse)

object Compressor615 extends HalfClbGpc(Seq(6, 1, 5).reverse)

object Compressor623 extends HalfClbGpc(Seq(6, 2, 3).reverse)

object Compressor1325 extends HalfClbGpc(Seq(1, 3, 2, 5).reverse)

object Compressor1415 extends HalfClbGpc(Seq(1, 4, 1, 5).reverse)

object Compressor1406 extends HalfClbGpc(Seq(1, 4, 0, 6).reverse)

object Compressor1407 extends HalfClbGpc(Seq(1, 4, 0, 7).reverse)

object Compressor2117 extends HalfClbGpc(Seq(2, 1, 1, 7).reverse)

// TODO: more GPC of other size

object Compressor6to3 extends PrimitiveGpc(Seq(6), Seq(1, 1, 1), Compressor6to3Primitive, VivadoUtilEstimation(lut = 3, ff = 3))

object Compressor3to2 extends PrimitiveGpc(Seq(3), Seq(1, 1), Compressor3to2Primitive, VivadoUtilEstimation(lut = 1, ff = 2))

object Gpcs {

  def apply(): Seq[Gpc] = Seq(
    Compressor6to3,
    Compressor3to2
//    Compressor606,
//    Compressor607,
//    Compressor615,
//    Compressor623,
//    Compressor1325,
//    Compressor1415,
//    Compressor1406,
//    Compressor1407,
//    Compressor2117
  )
}
