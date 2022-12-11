package Chainsaw.deprecated

import Chainsaw._
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

case class FineReduction(M: BigInt, upperBound: Int) extends ChainsawGenerator { //  override def name = getAutoName(this)

  override def name = getAutoName(this)

  val k = M.bitLength
  val widthIn = log2Up(upperBound) + k
  val widthOut = k
  val detWidth = log2Up(upperBound) + 1 // downto k-1
  val detTable: Seq[BigInt] = (0 until 1 << detWidth).map(msbValue => (BigInt(msbValue) << (k - 1)) / M)
  val sub0Gen, sub1Gen = CpaS2S(BinarySubtractor, widthIn, withCarry = true)

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = Seq(dataIn.head.asInstanceOf[BigInt].mod(M))

  override def generateTestCases = Seq.fill(1000)(BigInt(widthIn, Random)).filter(_ < upperBound * M)

  override def inputTypes = Seq(UIntInfo(widthIn))

  override def outputTypes = Seq(UIntInfo(widthOut))

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  override def latency = sub0Gen.latency + 4

  // TODO: optimization for [0, 2M) and (-M, M)
  override def implH: ChainsawModule = new ChainsawModule(this) {

    val multipleCount = detTable.distinct.length // number of different multiples needed
    val detRom = Mem(detTable.map(U(_, log2Up(multipleCount) bits)))
    val multipleRom = Mem((0 to detTable.max.toInt).map(value => U(M * value, widthIn bits)))

    val T = uintDataIn.head
    val det = T.takeHigh(detWidth).asUInt

    val multipleAddr = detRom.readSync(det)
    val multiple0 = multipleRom.readSync(multipleAddr).d() // \widetilde{Y}M
    val multiple1 = multipleRom.readSync(multipleAddr + 1).d() // (\widetilde{Y}+1)M

    // TODO: replace this with a ternary subtractor
    val ret0 = sub0Gen.asFunc(Seq(T.d(3), multiple0).map(_.asBits)).head // d(3) for readSync latency
    val ret1 = sub0Gen.asFunc(Seq(T.d(3), multiple1).map(_.asBits)).head

    dataOut.head := Mux(ret1.msb, ret0, ret1).d().resize(widthOut)
  }

  override def implNaiveH = Some(new ChainsawModule(this) {
    uintDataOut.head := (uintDataIn.head % M).d(latency)
  })
}
