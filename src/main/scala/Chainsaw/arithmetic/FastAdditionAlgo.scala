package Chainsaw.arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.util.Random
import Chainsaw._
import Chainsaw.xilinx._

import scala.collection.mutable.ArrayBuffer

abstract class FastAdditionAlgo extends HardAlgo {

  val width: Int

  def impl(a: BigInt, b: BigInt, cIn: BigInt): BigInt

  def selfTest(): Unit = Seq
    .fill(1000)(Seq.fill(2)(BigInt(width, Random)) :+ BigInt(1, Random))
    .foreach { case Seq(a, b, cin) =>
      assert(
        impl(a, b, cin) == (a + b + cin),
        s"$a + $b + $cin != ${impl(a, b, cin)}"
      )
    }
}

abstract class CarrySelectAddition extends FastAdditionAlgo {

  // TODO: the first work need no selection
  val blockWidth: Int

  def blockCount = width.divideAndCeil(blockWidth)

  def getWords(a: BigInt, b: BigInt) = {
    val aWords = a.toBitValue(width).subdivideIn(blockCount)
    //    println("aWords: " + aWords.mkString(" "))
    val bWords = b.toBitValue(width).subdivideIn(blockCount)
    //    println("bWords: " + bWords.mkString(" "))
    (aWords, bWords)
  }

  def ccc(cin: BigInt, c0s: Seq[BigInt], c1s: Seq[BigInt]) = {
    val cs = ArrayBuffer[BigInt](cin)
    c0s.indices.foreach(i =>
      if (c0s(i) == 0 && c1s(i) == 0) cs += 0            // kill/annihilated
      else if (c0s(i) == 0 && c1s(i) == 1) cs += cs.last // propagate
      else cs += 1                                       // generate
    )
    cs
  }

//  def combineWords(sWords: Seq[BigInt], carryOut: BigInt): BigInt =
//    (carryOut << (blockWidth * blockCount)) + sWords.zipWithIndex.map {
//      case (word, index) => word << (blockWidth * index)
//    }.sum

  def combineWords(sWords: Seq[BigInt], carryOut: BigInt): BigInt = {
    (carryOut.toBitValue(1) @@ sWords
      .map(_.toBitValue(blockWidth))
      .reverse
      .reduce(_ @@ _)).value
  }

}

case class AamAddition(width: Int, blockWidth: Int)
    extends CarrySelectAddition {

  override def impl(a: BigInt, b: BigInt, cin: BigInt) = {
    val (aWords, bWords) = getWords(a, b)
    val c0AndS0s = aWords
      .zip(bWords)
      .map { case (a, b) => a + b }
      .map(_.toBitValue(blockWidth + 1).splitAt(blockWidth))
    val c1AndS1s = aWords
      .zip(bWords)
      .map { case (a, b) => a + b + 1 }
      .map(_.toBitValue(blockWidth + 1).splitAt(blockWidth))
    val c0s = c0AndS0s.map(_._1) // low to high
    val s0s = c0AndS0s.map(_._2)
    val c1s = c1AndS1s.map(_._1)
    val s1s = c1AndS1s.map(_._2)
    // stage 2, CCC
    val cs = ccc(cin, c0s, c1s)
    // stage 3, MUXes
    val sWords =
      s0s.zip(s1s).zip(cs).map { case ((s0, s1), c) => if (c == 1) s1 else s0 }
    combineWords(sWords, cs.last)
  }

  override def vivadoUtilEstimation: VivadoUtil =
    VivadoUtil(lut = (3 * blockCount + 1) * blockWidth)
}

case class CaiAddition(width: Int, blockWidth: Int)
    extends CarrySelectAddition {

  override def impl(a: BigInt, b: BigInt, cin: BigInt) = {
    val (aWords, bWords) = getWords(a, b)
    val c0AndS0s = aWords
      .zip(bWords)
      .map { case (a, b) => a + b }
      .map(_.toBitValue(blockWidth + 1).splitAt(blockWidth))
    val c0s = c0AndS0s.map(_._1) // low to high
    val s0s = c0AndS0s.map(_._2)
    val c1s = aWords
      .zip(bWords)
      .map { case (a, b) => a >= ~b.toBitValue(blockWidth) }
      .map(if (_) BigInt(1) else BigInt(0))
    // stage 2, CCC
    val cs = ccc(cin, c0s, c1s)
    // stage 3, RCAs
    val sWords = s0s.zip(cs).map { case (s0, c) => s0 + c }
    combineWords(sWords, cs.last)
  }

  override def vivadoUtilEstimation: VivadoUtil =
    VivadoUtil(lut = (2.5 * blockCount + 1) * blockWidth)
}

class CcaAddition(val width: Int, val blockWidth: Int)
    extends CarrySelectAddition {

  override def impl(a: BigInt, b: BigInt, cin: BigInt) = {
    val (aWords, bWords) = getWords(a, b)
    println("aWords: " + aWords.mkString(" "))
    println("bWords: " + bWords.mkString(" "))
    // stage 1, RCAs and comparators
    val c0s = aWords
      .zip(bWords)
      .map { case (a, b) => a > ~b.toBitValue(blockWidth) }
      .map(if (_) BigInt(1) else BigInt(0))
    println("c0s: " + c0s.mkString(" "))
    val c1s = aWords
      .zip(bWords)
      .map { case (a, b) => a >= ~b.toBitValue(blockWidth) }
      .map(if (_) BigInt(1) else BigInt(0))
    println("c1s: " + c1s.mkString(" "))
    // stage 2, CCC
    val cs = ccc(cin, c0s, c1s)
    println("cs: " + cs.mkString(" "))
    // stage 3, RCAs
    val sWords = aWords.zip(bWords).zip(cs).map { case ((a, b), c) =>
      (a + b + c).mod(pow2(blockWidth))
    }
    println("sWords: " + sWords.mkString(" "))
    combineWords(sWords, cs.last)
  }

  // when stage 1 is pipelined, stage 2 & 3 are combined and pipelined
  override def vivadoUtilEstimation: VivadoUtil =
    VivadoUtil(lut = 2 * width + blockCount, ff = 4 * width)
}

object CcaAddition {
  def apply(width: Int, blockWidth: Int): CcaAddition =
    new CcaAddition(width, blockWidth)

  def main(args: Array[String]): Unit = {
    CcaAddition(8, 4).impl(53, 247, 1)
  }
}
