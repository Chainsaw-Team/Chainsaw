package Chainsaw.project.ChipVerify

import spinal.core._
import spinal.core.sim.{SpinalSimConfig, _}
import spinal.lib._

import scala.language.postfixOps

case class CxpVideoDma(stream: Int, streamWords: Int) extends Bundle with IMasterSlave {

  val data               = Bits(stream * streamWords * 32 bits)
  val valid, ready       = Bool
  val sop, eop, sol, eol = Bool // start of packet, end of packet, start of line, end of line
  val empty              = Bits(log2Up(4 * streamWords) bits) // empty words in the packet

  override def asMaster(): Unit = {
    out(data, sop, eop, sol, eol, empty, valid)
    in(ready)
  }

}

// this module would receive image data from CXP IP and send it to the host through xillybus
// this module insert headers according to the eop signal
case class ImageRx(height: Int, width: Int) extends Component {

  // I/O
  val dmaIn     = slave(CxpVideoDma(1, 4))
  val streamOut = master(Stream(Bits(32 bits)))
  val rxDone    = out Bool ()

  // params
  val header         = B(BigInt("ABCDABCD", 16))
  val pixelsPerCycle = dmaIn.streamWords * 4
  assert(width % pixelsPerCycle == 0)
  val cyclePerRow = width / pixelsPerCycle

  // using eop as last
  val data = dmaIn.data
  val data128WithLast = data(31 downto 0) ## B(0, 1 bits) ##
    data(63 downto 32) ## B(0, 1 bits) ##
    data(95 downto 64) ## B(0, 1 bits) ##
    data(127 downto 96) ## dmaIn.eop

  val streamIn = Stream(Bits(data128WithLast.getBitsWidth bits))
  streamIn.payload := data128WithLast
  streamIn.valid   := dmaIn.valid
  dmaIn.ready      := streamIn.ready

  // store up to 2 frames in buffer
  val bufferedStream = Stream(Bits(data128WithLast.getBitsWidth bits))
  val bufferDepth    = ((height + 4) * width * (10 + 1) / pixelsPerCycle) + 10
//  val bufferDepth = 1696 * 1710 / pixelsPerCycle + 10
  val frameBuffer = StreamFifo(data128WithLast, bufferDepth)
  print(s"buffer depth = $bufferDepth")
  streamIn           >> frameBuffer.io.push
  frameBuffer.io.pop >> bufferedStream

  // 128bit -> 32bit conversion
  val convertedStream = Stream(Bits((32 + 1) bits))
  StreamWidthAdapter(bufferedStream, convertedStream, BIG)

  convertedStream.translateWith(convertedStream.payload(32 downto 1)) >> streamOut

  frameBuffer.io.occupancy.addAttribute("mark_debug", "true").setName("frameBufferOccupancy")
  (~frameBuffer.io.push.ready).addAttribute("mark_debug", "true").setName("frameBufferFull")
  (~frameBuffer.io.pop.valid).addAttribute("mark_debug", "true").setName("frameBufferEmpty")

  val clear        = in Bool ()
  val pixelCounter = Counter(cyclePerRow, inc = dmaIn.valid)
  val rowCounter   = Counter(height, inc = pixelCounter.willOverflow)
  val frameCounter = Counter(20, inc = rowCounter.willOverflow)
  when(clear) {
    pixelCounter.clear()
    rowCounter.clear()
    frameCounter.clear()
  }

  pixelCounter.value.addAttribute("mark_debug", "true").setName("pixelCounter")
  rowCounter.value.addAttribute("mark_debug", "true").setName("rowCounter")
  frameCounter.value.addAttribute("mark_debug", "true").setName("frameCounter")

  // debug
  rxDone := rowCounter.willOverflow
  rxDone.addAttribute("mark_debug", "true").setName("rxDone")

}

object ImageRx {
  def main(args: Array[String]): Unit = {

    val height = 44
    val width  = 128

    SpinalSimConfig().withFstWave.compile(ImageRx(height, width)).doSim { dut =>
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
        dut.clear           #= true
        dut.clockDomain.waitSampling()
      }

      // simulating transferring 42 X 128 images
      def sendImage() = {
        dut.clear #= false
        val pixelsPerCycle = dut.pixelsPerCycle
        val cyclePerRow    = dut.cyclePerRow
        (0 until height).foreach { row =>
          (0 until cyclePerRow + 50).foreach { i =>
            val idx       = (row * width + i * pixelsPerCycle) % (1 << 8)
            val dataValue = (0 until pixelsPerCycle).map(j => BigInt(idx + j) << (j * 8)).sum
            dut.dmaIn.sol   #= (i == 0)
            dut.dmaIn.eol   #= (i == cyclePerRow - 1)
            dut.dmaIn.eop   #= (i == cyclePerRow - 1 && row == height - 1)
            dut.dmaIn.data  #= (if (i < cyclePerRow) dataValue else BigInt(0))
            dut.dmaIn.valid #= i < cyclePerRow
            if (i < cyclePerRow)
              println(
                s"idx = ${idx.hexString(8)}~${(idx + pixelsPerCycle - 1)
                  .hexString(8)}, eop = ${i == cyclePerRow - 1 && row == height - 1}"
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
