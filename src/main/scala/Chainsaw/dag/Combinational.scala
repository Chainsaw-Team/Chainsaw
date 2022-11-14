package Chainsaw.dag

import Chainsaw._
import spinal.core._

abstract class Combinational extends ChainsawGenerator {

  override var latency = 0

  override def implH = null

  def comb(dataIn:Seq[Bits]):Seq[Bits]
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

  override var inputTypes = Seq(UIntInfo(width))
  override var outputTypes = Seq(UIntInfo(width - lowWidth), UIntInfo(lowWidth))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
}

case class ShiftLeft(shift: Int, width: Int) extends Combinational {

  override def comb(dataIn: Seq[Bits]) = dataIn.map(_ << shift)

  override def name = s"shift_$shift"

  override def impl(dataIn: Seq[Any]) = dataIn.asInstanceOf[Seq[BigInt]].map(_ << shift)

  override var inputTypes = Seq(UIntInfo(width))
  override var outputTypes = Seq(UIntInfo(width + shift))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
}

case class Resize(widthIn: Int, widthOut: Int) extends Combinational {

  override def comb(dataIn: Seq[Bits]): Seq[Bits] = dataIn.map(_.resize(widthOut))

  override def name = s"resize_${widthIn}_$widthOut"

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = dataIn.asInstanceOf[Seq[BigInt]]

  override var inputTypes = Seq(UIntInfo(widthIn))
  override var outputTypes = Seq(UIntInfo(widthOut))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
}