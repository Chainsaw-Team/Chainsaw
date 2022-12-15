package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

case class Cpa(adderType: AdderType, width: Int)
  extends UnsignedMerge {

  override def inputTimes = inputTypes.map(_ => 0)

  override def outputTimes = Seq(0)

  override def arithInfos = {
    val signs = adderType match {
      case BinaryAdder => Seq(true, true)
      case BinarySubtractor => Seq(true, false)
      case TernaryAdder => Seq(true, true, true)
      case TernarySubtractor1 => Seq(true, true, false)
      case TernarySubtractor2 => Seq(true, false, false)
    }
    signs.map(sign => ArithInfo(width, 0, sign))
  }

  override def implH = ???

  override def latency() = width.divideAndCeil(cpaWidthMax)

  override def name = s"${className(adderType)}_$width"

  override def vivadoUtilEstimation = VivadoUtilEstimation(lut = width + 2, carry8 = (width + 2).divideAndCeil(8))

  override def fmaxEstimation = 600 MHz

  def sum(data: UInt*) = {
    // FIXME: use implH
    val core = implNaiveH.get
    core.dataIn.zip(data).foreach { case (in, data) => in := data.toAFix }
    core.dataOut.head.asUInt()
  }
}
