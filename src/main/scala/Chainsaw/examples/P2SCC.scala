package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

case class P2SCC(bitWidth: Int, factor: Int) extends Component {
  val slow                  = in Vec (Bits(bitWidth bits), factor)
  val fast                  = out Bits (bitWidth bits)
  val clkSlow, clkFast, rst = in Bool ()

  val domainSlow = ClockDomain(clkSlow, reset = rst)
  val domainFast = ClockDomain(clkFast, reset = rst)

  val fifo = StreamFifoCC(Bits(8 * factor bits), 8, domainSlow, domainFast)

  val streamIn = Stream(Bits(bitWidth * factor bits))
  streamIn.valid   := True
  streamIn.payload := slow.reduce(_ ## _)

  val streamOut = Stream(Bits(bitWidth bits))
  fast := streamOut.payload


  streamIn    >> fifo.io.push
  fifo.io.pop >> streamOut
}
