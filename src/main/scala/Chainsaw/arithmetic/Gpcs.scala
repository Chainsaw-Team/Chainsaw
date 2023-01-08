package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.arithmetic.bitheap.Bit
import Chainsaw.arithmetic.flopoco.{FlopocoBlackBox, XilinxGpc}
import Chainsaw.device.LUT6_2
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim.SpinalSimBackendSel.GHDL
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.math.BigInt

abstract class Gpc extends CompressorGenerator {

  def name =
    s"${className(this)}_${if (shouldDoComplement) hashName(getComplementHeap)
    else "noComplement"}"

  // column in
  override def inputTypes =
    inputFormat.filter(_ > 0).map(NumericType.U)

  // row out
  override def outputTypes = {
    require(outputFormat.max == 1)
    Seq(NumericType.U(outputFormat.length))
  }

  val inputWeights = inputFormat.zipWithIndex.filter(_._1 > 0).map(_._2)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    yours.head.toBigInt() == golden.head.toBigInt()
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
}

/** -------- built by Xilinx primitives
  * --------
  */

case class Compressor6to3(override val complementHeap: Seq[Seq[Boolean]] = null)
    extends Gpc {

  override def inputFormat = Seq.fill(1)(6)

  override def outputFormat = Seq.fill(3)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 3, ff = outputFormat.sum)

  override def implH = new ChainsawOperatorModule(this) {
    val lutValues = Seq(
      BigInt("6996966996696996", 16),
      BigInt("8117177e177e7ee8", 16),
      BigInt("fee8e880e8808000", 16)
    )
    val inverseList = getComplementHeap.head.padTo(6, true).map(!_)
    val dataBitsIn  = dataIn.head.raw.asBools
    val lutOuts = lutValues
      .map(LUT6_2.getValueWithInverse(_, inverseList))
      .map(LUT6_2.process(dataBitsIn, _).last)
    dataOut.head := lutOuts.asBits().asUInt.toAFix
  }
}

case class Compressor3to2(override val complementHeap: Seq[Seq[Boolean]] = null)
    extends Gpc {

  override def inputFormat = Seq.fill(1)(3)

  override def outputFormat = Seq.fill(2)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 1, ff = outputFormat.sum)

  override def implH = new ChainsawOperatorModule(this) {
    val lutValues = BigInt("96969696e8e8e8e8", 16)
    val inverseList =
      getComplementHeap.head.padTo(3, true).map(!_) ++ Seq.fill(3)(false)
    val dataBitsIn    = dataIn.head.raw.asBools // low to high
    val inversedValue = LUT6_2.getValueWithInverse(lutValues, inverseList)
    val lutOuts =
      LUT6_2.process(dataBitsIn ++ Seq(False, False, True), inversedValue)
    dataOut.head := lutOuts.reverse.asBits().asUInt.toAFix
  }
}

object Gpcs {
  def apply(): Seq[Gpc] = Seq(
    Compressor6to3(),
    Compressor3to2()
  )
}
