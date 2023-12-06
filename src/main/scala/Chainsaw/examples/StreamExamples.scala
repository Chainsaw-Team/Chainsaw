package Chainsaw.examples

import Chainsaw.arithmetic.floating._
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math.Floating
import spinal.lib.sim._
import spinal.lib.{Stream, _}

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

case class DelayLineWithBackPressure() extends Component {
  val dataIn  = slave Stream AFix.U(8 bits)
  val dataOut = master Stream AFix.U(8 bits)

  val delayedData = Seq.iterate(dataIn, 3)(_.m2sPipe())
  delayedData.last >> dataOut
}

case class StreamForkAndJoinExample() extends Component {
  val a, b, c = slave Stream AFix.U(8 bits)
  val d, e    = master Stream AFix.U(8 bits)

  val Seq(c0, c1) = StreamFork(c, 2, synchronous = true).map(_.m2sPipe())

  // datapath, datapath delay =
  val sum0 = a.payload +| c0.payload
  val sum1 = b.payload +| c1.payload

  StreamJoin(a, c0).translateWith(sum0).m2sPipe() >> d
  StreamJoin(b, c1).translateWith(sum1).m2sPipe() >> e

}

case class StreamMuxExample(safe: Boolean) extends Component {
  val a, b = slave Stream AFix.U(8 bits)
  val sel  = slave Stream AFix.U(1 bits)
  val c    = master Stream AFix.U(8 bits)

  sel.freeRun()
  StreamMux(sel.payload.asUInt(), Seq(a, b)).m2sPipe() >> c
}

case class StreamMuxSafeExample() extends Component {
  val a, b = slave Stream AFix.U(8 bits)
  val sel  = slave Stream AFix.U(1 bits)
  val c    = master Stream AFix.U(8 bits)

  StreamMuxSafe(sel.translateWith(sel.payload.asUInt()), Seq(a, b)).m2sPipe() >> c
}

object StreamMuxSafe {
  def apply[T <: Data](select: Stream[UInt], inputs: Seq[Stream[T]]): Stream[T] = {
    val vec = Vec(inputs)
    StreamMuxSafe(select, vec)
  }

  def apply[T <: Data](select: Stream[UInt], inputs: Vec[Stream[T]]): Stream[T] = {
    val c = new StreamMuxSafe(inputs(0).payload, inputs.length)
    (c.io.inputs, inputs).zipped.foreach(_ << _)
    select                                 >> c.io.select
    c.io.output
  }
}

class StreamMuxSafe[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val select = slave Stream UInt(log2Up(portCount) bit)
    val inputs = Vec(slave Stream (dataType), portCount)
    val output = master Stream (dataType)
  }
  for ((input, index) <- io.inputs.zipWithIndex) {
    input.ready := io.select.payload === index && io.output.ready && io.select.valid
  }
  io.output.valid   := io.inputs(io.select.payload).valid
  io.output.payload := io.inputs(io.select.payload).payload

  io.select.ready := io.inputs.map(_.ready).reduce(_ && _)
}

object StreamPokeFloating {
  def apply(
      stream: Stream[Floating],
      clockDomain: ClockDomain,
      data: Seq[Float],
      bandwidth: Double = 0.5,
      depth: Int        = Int.MaxValue
  ): Unit = {

    val unusedData = mutable.Stack(data: _*)
    val dataFifo   = mutable.Queue[Float]()

    var validLast = false
    var empty     = true

    def fireLast = validLast && stream.ready.toBoolean

    def iter() = {
      // source -> FIFO
      if (unusedData.nonEmpty) {
        val poke = Random.nextDouble() < bandwidth
        if (poke) dataFifo.enqueue(unusedData.pop())
      }
      if (fireLast) empty = true
      // FIFO -> DUT
      if (empty) { // update data
        if (dataFifo.nonEmpty) {
          stream.payload #= dataFifo.dequeue()
          stream.valid   #= true
          validLast = true
          empty     = false
        } else {
          stream.payload.randomize()
          stream.valid #= false
          validLast = false
        }
      }
      if (dataFifo.length > depth) simFailure("source buffer overflow")
    }

    stream.valid #= false
    clockDomain.onSamplings(iter())

  }
}

object StreamPoke {
  def apply(
      stream: Stream[AFix],
      clockDomain: ClockDomain,
      data: Seq[Double],
      bandwidth: Double = 0.5,
      depth: Int        = Int.MaxValue
  ): Unit = {

    val unusedData = mutable.Stack(data: _*)
    val dataFifo   = mutable.Queue[Double]()

    var validLast = false
    var empty     = true

    def fireLast = validLast && stream.ready.toBoolean

    def iter() = {
      // source -> FIFO
      if (unusedData.nonEmpty) {
        val poke = Random.nextDouble() < bandwidth
        if (poke) dataFifo.enqueue(unusedData.pop())
      }
      if (fireLast) empty = true
      // FIFO -> DUT
      if (empty) { // update data
        if (dataFifo.nonEmpty) {
          stream.payload #= dataFifo.dequeue()
          stream.valid   #= true
          validLast = true
          empty     = false
        } else {
          stream.payload.randomize()
          stream.valid #= false
          validLast = false
        }
      }
      if (dataFifo.length > depth) simFailure("source buffer overflow")
    }

    stream.valid #= false
    clockDomain.onSamplings(iter())

  }
}

object StreamPeekFloating {
  def apply(
      stream: Stream[Floating],
      clockDomain: ClockDomain,
      scoreboard: ScoreboardInOrder[Float],
      bandwidth: Double = 0.5
  ): Unit = {
    stream.ready #= false
    StreamReadyRandomizer(stream, clockDomain).factor = bandwidth.toFloat
    StreamMonitor(stream, clockDomain) { payload => scoreboard.pushDut(payload.toFloat) }
  }
}

object StreamPeek {
  def apply(
      stream: Stream[AFix],
      clockDomain: ClockDomain,
      scoreboard: ScoreboardInOrder[Double],
      bandwidth: Double = 0.5
  ): Unit = {
    stream.ready #= false
    StreamReadyRandomizer(stream, clockDomain).factor = bandwidth.toFloat
    StreamMonitor(stream, clockDomain) { payload => scoreboard.pushDut(payload.toDouble) }
  }
}
