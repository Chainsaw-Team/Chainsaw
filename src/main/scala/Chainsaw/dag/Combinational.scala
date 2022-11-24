package Chainsaw.dag

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

/** simple generators which are combinational for the ease of Dag implementation
 *
 */
abstract class Combinational extends ChainsawGenerator {

  override def latency = 0

  override def implH = null // this should never be invoked, instead, method comb will be used for implementation


  def comb(dataIn: Seq[Bits]): Seq[Bits]
}

case class Split(width: Int, lowWidth: Int) extends Combinational {

  def comb(dataIn: Seq[Bits]): Seq[Bits] = {
    val (a, b) = dataIn.head.splitAt(lowWidth)
    Seq(a, b)
  }

  override def name = s"split_${width}_$lowWidth"

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val (high, low) = dataIn.asInstanceOf[Seq[BigInt]].head.toBitValue().splitAt(lowWidth)
    Seq(high, low)
  }

  override def inputTypes = Seq(UIntInfo(width))
  override def outputTypes = Seq(UIntInfo(width - lowWidth), UIntInfo(lowWidth))

  override def inputFormat = inputNoControl
 override def outputFormat = outputNoControl
}

case class SplitN(width: Int, n: Int) extends Combinational {

  def comb(dataIn: Seq[Bits]): Seq[Bits] = dataIn.head.subdivideIn(n slices, strict = false)

  override def name = s"split_${width}_$n"

  override def impl(dataIn: Seq[Any]): Seq[BigInt] =
    dataIn.head.asInstanceOf[BigInt].toBitValue(width).splitN(n)

  override def inputTypes = Seq(UIntInfo(width))
  override def outputTypes = Seq.fill(n)(UIntInfo(width.divideAndCeil(n)))

  override def inputFormat = inputNoControl
 override def outputFormat = outputNoControl
}

case class ShiftLeft(shift: Int, width: Int) extends Combinational {

  override def comb(dataIn: Seq[Bits]) = dataIn.map(_ << shift)

  override def name = s"shift_$shift"

  override def impl(dataIn: Seq[Any]) = dataIn.asInstanceOf[Seq[BigInt]].map(_ << shift)

  override def inputTypes = Seq(UIntInfo(width))
  override def outputTypes = Seq(UIntInfo(width + shift))

  override def inputFormat = inputNoControl
 override def outputFormat = outputNoControl
}

case class Resize(widthIn: Int, widthOut: Int) extends Combinational {

  override def comb(dataIn: Seq[Bits]): Seq[Bits] = dataIn.map(_.resize(widthOut))

  override def name = s"resize_${widthIn}_$widthOut"

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = dataIn.asInstanceOf[Seq[BigInt]]

  override def inputTypes = Seq(UIntInfo(widthIn))
  override def outputTypes = Seq(UIntInfo(widthOut))

  override def inputFormat = inputNoControl
 override def outputFormat = outputNoControl
}

// TODO: pipeline for this
case class GetTiling(widthA: Int, widthB: Int) extends ChainsawGenerator {

  override def name = s"getTiling_${widthA}_$widthB"

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = dataIn.asInstanceOf[Seq[BigInt]]

  override def inputTypes = Seq(UIntInfo(widthA + 1), UIntInfo(widthB + 1))
  override def outputTypes = Seq(UIntInfo(widthA), UIntInfo(widthB),
    UIntInfo(widthA), UIntInfo(widthB), UIntInfo(1))

  override def inputFormat = inputNoControl
 override def outputFormat = outputNoControl

  override def latency = 1

  /** --------
   * implementations
   * -------- */
  override def implH = new ChainsawModule(this){
    val Seq(a, b) = uintDataIn
    val (aMsb, aMain) = a.splitAt(widthA)
    val (bMsb, bMain) = b.splitAt(widthB)
    val sideA = Mux(bMsb.asBool, aMain, B(0, widthA bits))
    val sideB = Mux(aMsb.asBool, bMain, B(0, widthB bits))
    dataOut := Seq(aMain, bMain, sideA, sideB, aMsb & bMsb).map(_.d())
  }
}

