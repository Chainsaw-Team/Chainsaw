package Chainsaw.xillybus

import Chainsaw._
import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow.boards.{PcieIntel, PcieXilinx}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

// TODO: do clockCrossing for output

/** wrapper for xillybus that generate FIFOs for each stream channel, and expose the ctrl interface for the memory-map
  * channel
  *
  * @param devices
  *   the device files you define through xillybus IP factory [[http://xillybus.com/custom-ip-factory]]
  */
case class XillybusWrapper(pinCount: Int, devices: Seq[XillybusDevice], target: ChainsawDevice, dataClockDomain: Option[ClockDomain] = None) extends Component {

  val pcieIntel  = target.isInstanceOf[AlteraDevice].generate(slave(PcieIntel(pinCount)))
  val pcieXilinx = target.isInstanceOf[XilinxDevice].generate(slave(PcieXilinx(pinCount)))

  val xillybus = Xillybus(pinCount, devices, target) // xillybus IP

  // global connections
  target match {
    case _: AlteraDevice => pcieIntel  <> xillybus.pcieIntel
    case _: XilinxDevice => pcieXilinx <> xillybus.pcieXilinx
  }

  val streamToHost   = xillybus.streamsRead.map(device => slave(Stream(Bits(device.bitWidth bits))))
  val streamFromHost = xillybus.streamsWrite.map(device => master(Stream(Bits(device.bitWidth bits))))

  assert(
    xillybus.memsBi.length + xillybus.memsRead.length + xillybus.memsWrite.length == 1,
    "only one mem device is supported"
  )
  val innerMemBus = xillybus.memBiInterfaces.headOption
    .getOrElse(
      xillybus.memWriteInterfaces.headOption
        .getOrElse(xillybus.memReadInterfaces.headOption.get)
    )
  val memBus = MemBi(xillybus.memsBi.head)
  memBus <> innerMemBus

  // xillybus IP <-> stream interface

  val pcieClockDomain = ClockDomain(
    clock = xillybus.bus_clk,
    reset = target match {
      case device: AlteraDevice => xillybus.pcieIntel.perstn
      case device: XilinxDevice => xillybus.pcieXilinx.PERST_B_LS
    },
    config = ClockDomainConfig(resetActiveLevel = LOW) // TODO: active-low for both Xilinx and Altera?
  )

  // creating buffers for read/write
  val dataClockDomainInUse = dataClockDomain.getOrElse(pcieClockDomain)
  pcieClockDomain on {
    xillybus.streamsRead.zip(xillybus.streamReadInterfaces).zip(streamToHost).foreach {
      case ((device, busSide), userSide) =>
        val buffer = StreamFifoCC(
          dataType  = HardType(busSide.data),
          depth     = device.getFifoDepth,
          pushClock = dataClockDomainInUse,
          popClock  = pcieClockDomain
        )

        println(f"fifoDepth for ${device.name} = ${device.getFifoDepth}")

        busSide.eof         := False            // unused
        userSide            >> buffer.io.push
        buffer.io.pop.ready := busSide.rden.d() // data should be valid on the following clock cycle of rden
        busSide.empty := buffer.io.popOccupancy <= U(1) // changed in advanced, as a read command may be in queue
        busSide.data  := buffer.io.pop.payload
    }

    xillybus.streamsWrite.zip(xillybus.streamWriteInterfaces).zip(streamFromHost).foreach {
      case ((device, busSide), userSide) =>
        val buffer = StreamFifoCC(
          dataType  = HardType(busSide.data),
          depth     = device.getFifoDepth,
          pushClock = pcieClockDomain,
          popClock  = dataClockDomainInUse
        )

        println(f"fifoDepth for ${device.name} = ${device.getFifoDepth}")

        buffer.io.pop          >> userSide
        buffer.io.push.valid   := busSide.wren
        busSide.full           := !buffer.io.push.ready
        buffer.io.push.payload := busSide.data
    }

  }

  def getStreamFromHost(name: String): Stream[Bits] =
    xillybus.streamsWrite
      .zip(streamFromHost)
      .find(_._1.name == name)
      .get
      ._2

  def getStreamToHost(name: String): Stream[Bits] =
    xillybus.streamsRead
      .zip(streamToHost)
      .find(_._1.name == name)
      .get
      ._2

  // TODO: need to upgrade ip
  if (target.isInstanceOf[XilinxDevice]) println("""
      |execute following tcl commands before synth_design:
      |read_verilog xillybus.v # from the corebundle generated
      |read_verilog xillybus_core.v # from the corebundle generated
      |read_verilog xillybus_core.edf # from the corebundle generated
      |import_ip $deviceFamily.xci # from the demo project for this family
      |""".stripMargin)

  if (target.isInstanceOf[AlteraDevice]) println("""
      |execute following tcl commands before synth_design:
      |set_global_assignment -name VERILOG_FILE xillybus.v # from the corebundle generated
      |set_global_assignment -name QXP_FILE xillybus_core.qxp # from the corebundle generated
      |set_global_assignment -name VERILOG_FILE pcie_c5_4x.v # from the demo project for this family
      |set_global_assignment -name QSYS_FILE pcie_reconfig.qsys # from the demo project for this family
      |""".stripMargin)
}

object XillybusWrapper {

  def defaultDevices: Seq[XillybusDevice] = Seq(
    XillybusFifoRead("read_32", 32),
    XillybusFifoWrite("write_32", 32),
    XillybusFifoRead("read_8", 8),
    XillybusFifoWrite("write_8", 8),
    XillybusMemBi("mem_32", 32, 16)
  )

  def defaultWrapper(pinCount: Int, device: ChainsawDevice): XillybusWrapper = XillybusWrapper(pinCount, defaultDevices, device)
}
