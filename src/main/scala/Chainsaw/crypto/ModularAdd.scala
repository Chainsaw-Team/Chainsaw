package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._

case class ModularAdd(width: Int, constantModulus: Option[BigInt], adderType: AdderType)
  extends ChainsawGenerator {

  override def name = s"modularAdd_$width"

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = adderType match {
      case BinaryAdder => data.sum.mod(constantModulus.get)
      case BinarySubtractor => (data(0) - data(1)).mod(constantModulus.get)
    }
    Seq(ret)
  }

  override var inputTypes = Seq.fill(2)(UIntInfo(width))
  override var outputTypes = Seq(UIntInfo(width))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  val cpaGen = CpaS2S(adderType, width, withCarry = true)
  val redcGen = FineReduction(constantModulus.get, 2)

  override var latency = cpaGen.latency + redcGen.latency

  override def implH = new ChainsawModule(this) {

    val cpa = cpaGen.implH
    val redc = redcGen.implH

    cpa.dataIn := dataIn
    redc.dataIn := cpa.dataOut
    dataOut := redc.dataOut
  }

}
