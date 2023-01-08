package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.math._

case class BitHeapGroup[T](bitHeaps: ArrayBuffer[BitHeap[T]]) {

  require(
    bitHeaps.map(_.time).distinct.length == bitHeaps.length,
    "all BitHeap must have different time value"
  )

  def weightLow = bitHeaps.map(_.weightLow).min

  def maxValue = bitHeaps.map(_.maxValue).sum

  def maxLength = maxValue.bitLength

  def positiveLength = maxValue.bitLength - weightLow

  def constant = bitHeaps.map(_.constant).sum

  def evalBigInt = bitHeaps.map(_.evalBigInt).sum

  /** -------- modifications
    * --------
    */

  def addConstant(constant: BigInt): Unit = bitHeaps.head.addConstant(constant)

  def absorbConstant(): Unit = {
    val exception0 = constant == 0
    val exception1 = constant < 0 && -constant >= pow2(maxLength)
    if (!exception0 && !exception1) {
      val constantToAdd = constant // fix the value
      bitHeaps.tail.foreach(_.constant = 0)
      val valueToAdd =
        if (constantToAdd >= 0) constant else pow2(maxLength) + constantToAdd
      if (constantToAdd >= 0) bitHeaps.head.constant = 0
      else bitHeaps.head.constant = -pow2(positiveLength) // clear constant
      bitHeaps.head.absorbPositiveConstant(valueToAdd)
      // making sure the negative prefix won't influence the final result
      if (constantToAdd < 0)
        require(
          -constant >= maxLength,
          s"${(-constant).bitLength}, ${maxLength}"
        )
    }
  }

  def implAllHard(solution: CompressorFullSolution): BitHeap[Bool] = {

    absorbConstant()

    val timeAndHeaps: Map[Int, BitHeap[Bool]] =
      bitHeaps.groupBy(_.time).map(pair => pair._1 -> pair._2.head.asHard)
    val stageSolutions = solution.stageSolutions.toBuffer

    var currentTime = timeAndHeaps.keys.min
    var currentHeap = timeAndHeaps(currentTime)

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
      } // else, nothing, as the implStageHard method is in-place
    }

    // no more following heaps
    stageSolutions.foreach(currentHeap.implStageHard)
    currentHeap
    //    currentHeap.implAllHard(CompressorFullSolution(stageSolutions))
  }

  override def toString = {
    bitHeaps.map(_.toString).mkString("\n--------next time step--------\n")
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

  def fromBigInts(
      weightedBigInts: Seq[WeightedBigInt]
  ): BitHeapGroup[BigInt] = {
    val bitHeaps = ArrayBuffer[BitHeap[BigInt]]()
    weightedBigInts.groupBy(_.arithInfo.time).foreach {
      case (_, weightedBigInts) =>
        bitHeaps += BitHeap.fromBigInts(weightedBigInts)
    }
    BitHeapGroup(bitHeaps)
  }

  def fromInfos(infos: Seq[ArithInfo]) = {
    val bitHeaps = ArrayBuffer[BitHeap[BigInt]]()
    infos.groupBy(_.time).map { case (_, info) =>
      bitHeaps += BitHeap.fromInfos(info)
    }
    BitHeapGroup(bitHeaps)
  }

  def apply[T](
      bitHeap: ArrayBuffer[ArrayBuffer[Bit[T]]],
      weightLow: Int,
      time: Int
  ): BitHeapGroup[T] =
    BitHeapGroup(ArrayBuffer(BitHeap(bitHeap, weightLow, time)))
}
