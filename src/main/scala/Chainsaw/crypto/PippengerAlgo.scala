package Chainsaw.crypto

import Chainsaw._
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

/** Pippenger algo is a algorithm for fast sum of scalar product
  */
case class PippengerAlgo[T: ClassTag](
    numberOfScalars: Int,
    scalarWidth: Int,
    windowWidth: Int,
    add: (T, T) => T,
    dbl: T      => T,
    negate: T   => T,
    latency: Int, // latency of fully-pipelined add and dbl operation
    zero: T
) {

  case class FIFO[D](size: Int) {
    val container = ArrayBuffer[D]()
    def push(t: D): Unit = {
      container += t
      if (container.length > size)
        throw new IllegalArgumentException("FIFO overflow")
    }
    def pop(): D      = container.remove(0)
    def getOccupation = container.length
    def getAll        = container
    def isFull        = container.length == size
    def isEmpty       = container.isEmpty
    def nonEmpty      = container.nonEmpty
  }

  case class RAM(size: Int) {
    val container                        = Array.fill(size)(zero)
    def write(index: Int, data: T): Unit = container(index) = data
    def read(index: Int): T              = container(index)
  }

  case class Pipeline[D](initial: Seq[D]) { // pipeline/history
    val container = ArrayBuffer[D](initial: _*)
    def tick(input: D): D = {
      container += input
      container.remove(0)
    }
    def contains(data: D) = container.contains(data)
  }

  def scalarMult(k: BigInt, point: T): T = {
    if (k == BigInt(0)) zero
    else {
      var current = point
      k.toString(2)
        .tail
        .foreach(char =>
          if (char == '1') current = add(dbl(current), point)
          else current             = dbl(current)
        )
      current
    }
  }

  val windowCount    = scalarWidth.divideAndCeil(windowWidth)
  val msbWindowWidth = scalarWidth % windowWidth
  // current scheme requires msbWindowWidth < windowWidth so that msb word won't overflow after signed digit conversion
  require(msbWindowWidth < windowWidth)
  val addrInWindow = 1 << (windowWidth - 1)
  val addrCount    = windowCount * addrInWindow
  val uramForAddr  = addrCount.divideAndCeil(4096)
  val uramForWidth = 1508.divideAndCeil(72)
  logger.info(s"addr count = $addrCount = URAM depth * $uramForAddr")
  logger.info(s"width = 1508 = URAM width * $uramForWidth")
  logger.info(s"URAM cost = ${uramForAddr * uramForWidth}")

  def impl(scalars: Seq[BigInt], points: Seq[T]) = {

    var addCount, dblCount, bubbleCount = 0
    def addWithRecord(a: T, b: T) = {
      val result = add(a, b)
      addCount += 1
      result
    }
    def dblWidthRecord(a: T) = {
      val result = dbl(a)
      dblCount += 1
      result
    }

    /** -------- data structures
      * --------
      */
    // BigInt for scalars, T for points, Int for address
    // data provider
    val scalarFIFO = FIFO[BigInt](numberOfScalars)
    val pointFIFO  = FIFO[T](numberOfScalars)
    scalars.foreach(scalarFIFO.push)
    points.foreach(pointFIFO.push)

    // record of past XXX cycles
    val history = Pipeline(Seq.fill(latency)((0, false))) // (address, valid)
    // the pipeline inside the adder
    val pipeline = Pipeline(Seq.fill(latency)(zero))

    val bucket                          = RAM(windowCount * addrInWindow)
    def getAddr(window: Int, word: Int) = window * addrInWindow + word

    val stallFIFO = FIFO[(T, Int)](4 * windowCount)

    var time = 0L
    def msbWindowExpansionFactor =
      time % (1 << (windowWidth - msbWindowWidth - 1))

    def getSignedWords(scalar: BigInt) = { // this trick halves the number of buckets
      val words = ArrayBuffer(
        scalar.toBitValue(scalarWidth).subdivideIn(windowCount): _*
      )

      words.indices.init.foreach { i =>
        if (words(i) >= (BigInt(1) << (windowWidth - 1))) {
          // [-2^(B-1), 2^(B-1)-1]
          words(i) = words(i) - (1 << windowWidth)
          // [0, 2^B], after following steps -> [-2^(B-1), 2^(B-1)]
          words(i + 1) += 1
        }
      }

      // msb window expansion
      words(words.length - 1) =
        if (words.last == 0) 0 // keep it as it is special
        else msbWindowExpansionFactor * (1 << msbWindowWidth) + words.last

      words
    }

    def fifoFlag =
      (stallFIFO.getOccupation > stallFIFO.size - windowCount) || scalarFIFO.isEmpty

    // three situations
    def doAdd(point: T, addr: Int): (T, Int, Boolean) = {
      val partial = bucket.read(addr) // read from bucket
      val sum     = addWithRecord(partial, point)
      (sum, addr, true)
    }

    def doStall(point: T, addr: Int): (T, Int, Boolean) = {
      stallFIFO.push((point, addr))
      bubbleCount += 1
      (zero, addr, false)
    }

    def doEmpty() = {
      bubbleCount += 1
      (zero, 0, false)
    }

    var all       = 0
    var conflicts = 0
    def checkConflict(addr: Int): Boolean = {
      val ret = history.contains((addr, true))
      if (ret) {
        conflicts += 1
        if (verbose >= 1)
          println(
            s"conflict at ${addr / addrInWindow}th window, ${addr % addrInWindow}th word"
          )
      }
      all += 1
      ret
    }

    def doCycle(point: T, addr: Int): Unit = {
      val (pointIn, addrIn, validIn) =
        if (addr < 0) doEmpty()
        else if (checkConflict(addr)) doStall(point, addr)
        else doAdd(point, addr)
      // pipeline -> write to bucket
      val pointOut            = pipeline.tick(pointIn)
      val (addrOut, validOut) = history.tick((addrIn, validIn))
      if (validOut) bucket.write(addrOut, pointOut)
      time += 1
    }

    // on FPGA, bucket aggregation
    while (scalarFIFO.nonEmpty || stallFIFO.nonEmpty) {
      if (!fifoFlag) {
        val scalar = scalarFIFO.pop()
        val point  = pointFIFO.pop()
        val words  = getSignedWords(scalar)

        words.zipWithIndex.foreach { case (word, window) => // for each word
          val trueWord = word.abs
          assert(trueWord >= 0 && trueWord <= (1 << (windowWidth - 1)))
          val truePoint = if (word < 0) negate(point) else point
          val addr =
            if (trueWord == 0) -1 else getAddr(window, trueWord.toInt - 1)
          doCycle(truePoint, addr)
        }
      } else { // for each FIFO
        (0 until stallFIFO.getOccupation).foreach { _ =>
          val (point, addr) = stallFIFO.pop()
          doCycle(point, addr)
        }
      }
    }

    (0 until latency).foreach { _ =>
      val point         = pipeline.tick(zero)
      val (addr, valid) = history.tick((0, false))
      if (valid) bucket.write(addr, point)
    }

    // on CPU

    val windowSums = bucket.container
      .grouped(addrInWindow)
      .zipWithIndex
      .map { case (bucket, i) =>
        bucket.zipWithIndex
          .map { case (point, k) =>
            val weight =
              if (i == windowCount - 1) k % (1 << msbWindowWidth)
              else k
            // + 1 as bucket index starts from 1
            scalarMult(BigInt(weight + 1), point)
          }
          .reduce(add)
      }

    val result = windowSums.zipWithIndex
      .map { case (point, i) =>
        Seq.iterate(point, i * windowWidth + 1)(dbl).last
      }
      .reduce(add)

    val golden = scalars
      .zip(points)
      .map { case (scalar, point) => scalarMult(scalar, point) }
      .reduce(add)

    assert(result == golden, s"result: $result, golden: $golden")
    assert(time == addCount + dblCount + bubbleCount)
    logger.info(
      s"\nwindowWidth: $windowWidth, windowCount: $windowCount, estimated addCount = windowCount * number of scalars = ${windowCount * numberOfScalars}" +
        s"\naddCount: $addCount, dblCount: $dblCount, bubbleCount: $bubbleCount, idle = ${bubbleCount.toDouble / time}, conflict = ${conflicts.toDouble / all}"
    )
    result
  }
}

// TODO: explore how the efficiency varies with different number of scalars and depth of FIFO
object PippengerAlgo extends App {
  Random.setSeed(42)
  val primeWidth      = 16
  val prime           = BigInt(16, Random)
  val scalarWidth     = 253
  val numberOfScalars = 10000
  val pippengerAlgo = PippengerAlgo(
    numberOfScalars,
    scalarWidth,
    windowWidth = 12,
    add         = (a: BigInt, b: BigInt) => (a + b).mod(prime),
    dbl         = (a: BigInt) => (a * 2).mod(prime),
    negate      = (a: BigInt) => -a.mod(prime),
    latency     = 225,
    zero        = BigInt(0)
  )

  val scalars = Seq.fill(numberOfScalars)(BigInt(scalarWidth, Random))
  val points  = Seq.fill(numberOfScalars)(BigInt(primeWidth, Random))
  pippengerAlgo.impl(scalars, points)

}
