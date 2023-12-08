package Chainsaw.project.ChipVerify

import spinal.core._
import spinal.core.sim.SpinalSimConfig
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

import scala.language.postfixOps

case class CxpVideoDma(stream: Int, streamWords: Int) extends Bundle with IMasterSlave {

  val data               = Bits(stream * streamWords * 32 bits)
  val valid, ready       = Bool
  val sop, eop, sol, eol = Bool
  val empty              = Bits(log2Up(4 * streamWords) bits)

  override def asMaster(): Unit = {
    out(data, sop, eop, sol, eol, empty, valid)
    in(ready)
  }

}

// this module would receive image data from CXP IP and send it to the host through xillybus
// this module insert headers according to the eop signal
case class ImageRx() extends Component {

  // I/O
  val dmaIn     = slave(CxpVideoDma(1, 4))
  val streamOut = master(Stream(Bits(32 bits)))
  val done      = out Bool ()

  // params
  val header = B(BigInt("ABCDABCD", 16))

  // using eop as last
  val data = dmaIn.data
  val dataWithLast = data(31 downto 0) ## B(0, 1 bits) ##
    data(63 downto 32) ## B(0, 1 bits) ##
    data(95 downto 64) ## B(0, 1 bits) ##
    data(127 downto 96) ## dmaIn.eop

  val StreamIn = Stream(Bits(dataWithLast.getBitsWidth bits))
  StreamIn.payload := dataWithLast
  StreamIn.valid   := dmaIn.valid
  dmaIn.ready      := StreamIn.ready // TODO: using deeper buffer and make it a free-run stream?

  val bufferedStream  = StreamIn.queue(32) // in case CXP IP ready doesn't work
  val convertedStream = Stream(Bits((32 + 1) bits))

  StreamWidthAdapter(bufferedStream, convertedStream, BIG)

  val streamWithLast = Stream(Fragment(Bits(32 bits)))
  streamWithLast.valid    := convertedStream.valid
  streamWithLast.fragment := convertedStream.payload(32 downto 1)
  streamWithLast.last     := convertedStream.payload(0)
  convertedStream.ready   := streamWithLast.ready

  done := streamWithLast.last // TODO: including image threshold determination

  val streamWithHeader = streamWithLast.insertHeader(header)
  streamWithHeader.freeRun()
  streamOut.payload := streamWithHeader.fragment
  streamOut.valid   := streamWithHeader.valid

  // debug
  val badEop    = out(dmaIn.eop & !dmaIn.valid)
  val headerOut = out((streamOut.payload === header & streamOut.valid).rise())
  assert(!badEop)

}

object ImageRx {
  def main(args: Array[String]): Unit = {
    SpinalSimConfig().withFstWave.compile(ImageRx()).doSim { dut =>
      def init() = {
        dut.clockDomain.forkStimulus(2)
        dut.dmaIn.data      #= 0
        dut.dmaIn.valid     #= false
        dut.dmaIn.sop       #= false
        dut.dmaIn.sol       #= false
        dut.dmaIn.eop       #= false
        dut.dmaIn.eol       #= false
        dut.dmaIn.empty     #= 0
        dut.streamOut.ready #= true // free run during simulation
        dut.clockDomain.waitSampling()
      }

      // simulating transferring 42 X 128 images
      def sendImage() = {
        val height         = 42
        val width          = 128
        val pixelsPerCycle = dut.dmaIn.streamWords * 4
        val rowCycle       = width / pixelsPerCycle
        (0 until height).foreach { row =>
          (0 until rowCycle + 50).foreach { i =>
            val idx       = (row * width + i * pixelsPerCycle) % (1 << 8)
            val dataValue = (0 until pixelsPerCycle).map(j => BigInt(idx + j) << (j * 8)).sum
            dut.dmaIn.sol   #= (i == 0)
            dut.dmaIn.eol   #= (i == rowCycle - 1)
            dut.dmaIn.eop   #= (i == rowCycle - 1 && row == height - 1)
            dut.dmaIn.data  #= (if (i < rowCycle) dataValue else BigInt(0))
            dut.dmaIn.valid #= i < rowCycle
            if (i < rowCycle)
              println(
                s"idx = ${idx.hexString(8)}~${(idx + pixelsPerCycle - 1).hexString(8)}, eop = ${i == rowCycle - 1 && row == height - 1}"
              )

            dut.clockDomain.waitSampling()
          }
        }
      }

      init()
      sendImage()
      sendImage()

      dut.clockDomain.waitSampling(100)

    }
  }
}
