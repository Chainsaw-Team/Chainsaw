package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.math._

/** this class is used to construct the [[BitHeapGroup]] model with some [[BitHeap]] which have different arrival time
  * @param bitHeaps
  *   the arrayBuffer which contain all [[BitHeap]] with different arrival time
  * @tparam T
  *   the raw data type, it can be HardType or SoftType
  */
case class BitHeapGroup[T](bitHeaps: ArrayBuffer[BitHeap[T]]) {

  require(
    bitHeaps.map(_.time).distinct.length == bitHeaps.length,
    "all BitHeap must have different time value"
  )

  /** this class is used to get the weightLow of this [[BitHeapGroup]], this weightLow is the minimum weightLow in all
    * different arrival time
    * @return
    *   the weightLow of this BitHeapGoup
    */
  def weightLow: Int = bitHeaps.map(_.weightLow).min

  /** the method is used to get the maximum value which this [[BitHeapGroup]] can represent(include stored constant)
    * @return
    *   the maximum value which this [[BitHeapGroup]] can represent
    */
  def maxValue: BigInt = bitHeaps.map(_.maxValue).sum

  /** the method is used to get the bitLength of the maxValue of this [[BitHeapGroup]]
    * @return
    *   the bitLength of the maxValue of this [[BitHeapGroup]]
    */
  def maxLength = maxValue.bitLength

  /** the method is used to get the bitLength of the maximum value which the nonEmpty Bit in this BitHeapGroup(exclude
    * the weightLow) can represent
    * @return
    *   the bitLength of nonEmpty Bit in this [[BitHeapGroup]](exclude the weightLow)
    */
  def positiveLength = maxValue.bitLength - weightLow

  /** the method is used to get the sum of constants in all [[BitHeap]]
    * @return
    *   the sum of constants in all [[BitHeap]]
    */
  def constant: BigInt = bitHeaps.map(_.constant).sum

  /** the method is used to get the sum of real value of every [[BitHeap]]
    * @return
    *   the sum of real value of every [[BitHeap]]
    */
  def evalBigInt = bitHeaps.map(_.evalBigInt).sum

  /** the method is used to indicate whether this [[BitHeapGroup]] is already reach final compress stage
    * @return
    *   the Boolean which indicate whether this [[BitHeapGroup]] is already reach final compress stage
    */
  def reachLastStage: Boolean = bitHeaps.forall(_.reachLastStage)

  /* -------- modifications -------- */

  /** the method is used to add a constant to the first [[BitHeap]] of [[BitHeapGroup]]
    * @param constant
    *   the constant will be added
    */
  def addConstant(constant: BigInt): Unit = bitHeaps.head.addConstant(constant)

  /** this method is used to absorb the constant in this [[BitHeapGroup]], the constant will convert to bits and add to
    * this BitHeapGroup
    */
  def absorbConstant(): Unit = {
    val exception0 = constant == 0
    val exception1 = constant < 0 && (-constant).mod(pow2(maxLength)) == 0
    if (!exception0 && !exception1) {
      val constantToAdd = constant // fix the value
      bitHeaps.tail.foreach(_.constant = 0)
      val valueToAdd =
        if (constantToAdd >= 0) constantToAdd else pow2(maxLength) - (-constantToAdd).mod(pow2(maxLength))
      if (constantToAdd >= 0) bitHeaps.head.constant = 0
      else
        bitHeaps.head.constant =
          constantToAdd - pow2(maxLength) + (-constantToAdd).mod(pow2(maxLength)) // clear constant
      bitHeaps.head.absorbPositiveConstant(valueToAdd)
      // making sure the negative prefix won't influence the final result
      if (constantToAdd < 0)
        require(
          -constant >= maxLength,
          s"${(-constant).bitLength}, ${maxLength}"
        )
    }
  }

  /** this method is used to compress this HardType [[BitHeapGroup]] by all compressors which parsing from
    * [[CompressorFullSolution]]
    * @param solution
    *   a [[CompressorFullSolution]] to guide this compress
    * @return
    *   the HardType [[BitHeap]] output of final stage compress
    */
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
            heapNext.absorbHeapFrom(currentHeap)
            currentHeap = heapNext
          case None =>
        }
        currentTime += 1
      } // else, nothing, as the implStageHard method is in-place
    }

    // no more following heaps
    stageSolutions.foreach(currentHeap.implStageHard)
    currentHeap
    //    currentHeap.implAllHard(CompressorFullSolution(stageSolutions))
  }

  /** override the toString method, it is used to visualize this [[BitHeapGroup]]
    * @return
    *   the visualized String of this [[BitHeapGroup]]
    */
  override def toString = {
    bitHeaps
      .sortBy(_.time)
      .map(_.toString)
      .mkString("\n--------next time step--------\n")
  }

  /** this method is used to deep copy this [[BitHeapGroup]]
    * @return
    *   the deep copy of this [[BitHeapGroup]]
    */
  def copy = BitHeapGroup(bitHeaps.map(_.copy))
}

object BitHeapGroup {

  /* ------- for merge arithmetic ------- */

  /** this method is used to construct a HardType [[BitHeapGroup]] according to given UInt information
    * @param weightedUInts
    *   the UInts(rows) which contain weight information for construct HardType [[BitHeapGroup]]
    * @return
    *   the HardType [[BitHeapGroup]] constructed from given information
    */
  def fromUInts(weightedUInts: Seq[WeightedUInt]): BitHeapGroup[Bool] = {
    val bitHeaps = ArrayBuffer[BitHeap[Bool]]()
    weightedUInts.groupBy(_.arithInfo.time).foreach { case (_, weightedUInts) =>
      bitHeaps += BitHeap.fromUInts(weightedUInts)
    }
    BitHeapGroup(bitHeaps)
  }

  /** this method is used to construct a SoftType [[BitHeapGroup]] according to given BigInts information
    * @param weightedBigInts
    *   the BigInts(rows) which contain weight information for construct SoftType [[BitHeapGroup]]
    * @return
    *   the SoftType [[BitHeapGroup]] constructed from given information
    */
  def fromBigInts(
      weightedBigInts: Seq[WeightedBigInt]
  ): BitHeapGroup[BigInt] = {
    val bitHeaps = ArrayBuffer[BitHeap[BigInt]]()
    weightedBigInts.groupBy(_.arithInfo.time).foreach { case (_, weightedBigInts) =>
      bitHeaps += BitHeap.fromBigInts(weightedBigInts)
    }
    BitHeapGroup(bitHeaps)
  }

  /** this method is used to construct a SoftType [[BitHeapGroup]] according to given [[ArithInfo]] information
    * @param infos
    *   the arithInfos(rows) which contain weight information for construct SoftType [[BitHeapGroup]]
    * @return
    *   the SoftType [[BitHeapGroup]] constructed from given information
    */
  def fromInfos(infos: Seq[ArithInfo]) = {
    val bitHeaps = ArrayBuffer[BitHeap[BigInt]]()
    infos.groupBy(_.time).map { case (_, info) =>
      bitHeaps += BitHeap.fromInfos(info)
    }
    BitHeapGroup(bitHeaps)
  }

  /** the method for constructing the [[BitHeapGroup]] which only one [[BitHeap]]
    * @param bitHeap
    *   the raw data, it should be a two-dimensional ArrayBuffer of Bit type
    * @param weightLow
    *   the minimum Bit weight of [[BitHeap]]
    * @param time
    *   the arrive time of this [[BitHeap]]
    * @tparam T
    *   the data type of Bit, it should be HardType or SoftType
    * @return
    *   the [[BitHeapGroup]] constructed from given information
    */
  def apply[T](
      bitHeap: ArrayBuffer[ArrayBuffer[Bit[T]]],
      weightLow: Int,
      time: Int
  ): BitHeapGroup[T] =
    BitHeapGroup(ArrayBuffer(BitHeap(bitHeap, weightLow, time)))
}
