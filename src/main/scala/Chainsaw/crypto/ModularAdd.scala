package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._

import scala.util.Random

// TODO: this should be implemented automatically by CPA + FineReduction
case class ModularAdd(width: Int, constantModulus: Option[BigInt], adderType: AdderType)
  extends ChainsawGenerator {

  constantModulus match {
    case Some(modulus) => require(modulus.bitLength <= width)
    case None => ???
  }

  override def name = getAutoName(this)

  val cpaGen = CpaS2S(adderType, width, withCarry = true)
  val redcGen = FineReduction(constantModulus.get, 2)
  logger.info(redcGen.M.bitLength.toString)

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = redcGen.impl(cpaGen.impl(dataIn))

  override var inputTypes = constantModulus match {
    case Some(_) => Seq.fill(2)(UIntInfo(width))
    case None => Seq.fill(3)(UIntInfo(width))
  }
  override var outputTypes = Seq(UIntInfo(width))

  override def generateTestCases = constantModulus match {
    case Some(modulus) => Seq.fill(1000)(inputWidths.map(width => BigInt(width, Random)))
      .filter(_.forall(_ < modulus))
      .flatten
    case None => ???
  }

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  override var latency = cpaGen.latency + redcGen.latency

  override def implH = new ChainsawModule(this) {
    val cpa = cpaGen.implH
    val redc = redcGen.implH

    cpa.dataIn := dataIn
    redc.dataIn := cpa.dataOut
    dataOut := redc.dataOut
  }

  override def implNaiveH = Some(new ChainsawModule(this) {
    constantModulus match {
      case Some(modulus) =>
        val ret = adderType match {
          case BinaryAdder => uintDataIn.reduce(_ +^ _) % modulus
          case BinarySubtractor => (uintDataIn.reduce(_ - _) % modulus).resize(width)
        }
        uintDataOut.head := ret.d(latency)
      case None => ???
    }
  })
}
