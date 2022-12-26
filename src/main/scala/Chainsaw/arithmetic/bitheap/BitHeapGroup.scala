package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.math._

case class BitHeapGroup[T](bitHeaps: ArrayBuffer[BitHeap[T]]) {

  require(bitHeaps.map(_.time).distinct.length == bitHeaps.length, "all BitHeap must have different time value")

  def weightLow = bitHeaps.map(_.weightLow).min

  def maxValue = bitHeaps.map(_.maxValue).sum

  def precision = Pow2(weightLow)

  /** -------- modifications
    * --------
    */

  private def representable(value: BigInt) = value.mod(precision) == 0

  def addNegativeConstant(constant: BigInt) = {
    require(constant < 0 && representable(-constant))
    require(maxValue >= -constant, "meaningless to add a negative constant larger than the max value")
    val validLength = (maxValue + constant).bitLength
    val complement  = Pow2(validLength) + constant
    bitHeaps.head.addPositiveConstant(complement)
    validLength
  }

  def implAllHard(solution: CompressorFullSolution) = {

    val timeAndHeaps: Map[Int, BitHeap[Bool]] =
      bitHeaps.groupBy(_.time).map(pair => pair._1 -> pair._2.head.asHard)
    val stageSolutions = solution.stageSolutions.toBuffer

    var currentTime = timeAndHeaps.keys.min
    var currentHeap = timeAndHeaps(currentTime)

    // FIXME: consider the case when current stage solution contains more no pipeline
    while (currentTime < timeAndHeaps.keys.max) {
      val currentStageSolution = stageSolutions.remove(0)
      currentHeap.implStageHard(currentStageSolution)
      if (currentStageSolution.pipelined) {
        timeAndHeaps.get(currentTime + 1) match {
          case Some(heapNext) =>
            heapNext.absorbHeapFrom(currentHeap.dSoft())
            currentHeap = heapNext
          case None =>
            currentHeap = currentHeap.dSoft()
        }
        currentTime += 1
      }
    }

    currentHeap.implAllHard(CompressorFullSolution(stageSolutions))
  }
}

object BitHeapGroup {

  /** for merge arithmetic
    */
  def fromUInts(weightedUInts: Seq[WeightedUInt]): BitHeapGroup[Bool] = {
    val bitHeaps = ArrayBuffer[BitHeap[Bool]]()
    weightedUInts.groupBy(_.arithInfo.time).foreach { case (_, weightedUInts) =>
      bitHeaps += BitHeap.fromUInts(weightedUInts)
    }
    BitHeapGroup(bitHeaps)
  }

  def fromBigInts(weightedBigInts: Seq[WeightedBigInt]): BitHeapGroup[BigInt] = {
    val bitHeaps = ArrayBuffer[BitHeap[BigInt]]()
    weightedBigInts.groupBy(_.arithInfo.time).foreach { case (_, weightedBigInts) =>
      bitHeaps += BitHeap.fromBigInts(weightedBigInts)
    }
    BitHeapGroup(bitHeaps)
  }

  def fromInfos(infos: Seq[ArithInfo]) = {
    val bitHeaps = ArrayBuffer[BitHeap[BigInt]]()
    infos.groupBy(_.time).map { case (_, info) => bitHeaps += BitHeap.fromInfos(info) }
    BitHeapGroup(bitHeaps)
  }

  def apply[T](bitHeap: ArrayBuffer[ArrayBuffer[Bit[T]]], weightLow: Int, time: Int): BitHeapGroup[T] =
    BitHeapGroup(ArrayBuffer(BitHeap(bitHeap, weightLow, time)))
}
