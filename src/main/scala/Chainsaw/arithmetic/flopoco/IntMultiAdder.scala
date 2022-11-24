package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core.{IntToBuilder, _}
import spinal.lib._

import scala.language.postfixOps

case class IntMultiAdder(widthIn: Int, n: Int, signed: Boolean) extends Flopoco {
  override val operatorName = "IntMultiAdder"
  override val params = Seq(("signedIn", if (signed) 1 else 0), ("n", n), ("wIn", widthIn))
  override val frequency = 800 MHz
  override val family = UltraScale

  override def inputTypes = Seq.fill(n)(UIntInfo(widthIn))
  val widthOut = inputTypes.head.bitWidth + log2Up(n)
  override def outputTypes = Seq(UIntInfo(widthOut))

  override def inputFormat = inputNoControl
 override def outputFormat = outputNoControl

  override def blackbox: FlopocoBlackBox = new FlopocoBlackBox {
    val X = in Vec(Bits(widthIn bits), n)
    X.zipWithIndex.foreach { case (int, i) => int.setName(s"X$i") }
    val R = out Bits (widthOut bits)

    override def asFunc: Seq[Bits] => Seq[Bits] = (dataIn: Seq[Bits]) => {
      val core = this
      core.X := Vec(dataIn)
      Seq(core.R)
    }
  }

  override def model(dataIn: Seq[Bits]) =
    if (!signed) Seq(dataIn.map(_.asUInt).reduceBalancedTree(_ +^ _).asBits)
    else Seq(dataIn.map(_.asSInt).reduceBalancedTree(_ +^ _).asBits)

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)

  flopocoDone()
}

object IntMultiAdder extends App {
  ChainsawSynth(IntMultiAdder(100, 20, signed = false), "synthFlopocoMultiAdder")
//  ChainsawImpl(IntMultiAdder(100, 20, signed = false), "FlopocoMultiAdder")
}