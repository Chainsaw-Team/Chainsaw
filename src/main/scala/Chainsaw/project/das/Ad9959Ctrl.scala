package Chainsaw.project.das

import Chainsaw.DataUtil
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.regif.{AccessType, BusIf, SymbolName}
import spinal.lib.fsm._

import scala.language.postfixOps

sealed trait ConfigItem { // TODO: make this a Chainsaw feature

}

case class ConstantConfig(value: BigInt) extends ConfigItem

case class RegConfig(defaultValue: BigInt) extends ConfigItem

case class Ad9959Config(
    frequencyDividerRatio: ConfigItem = RegConfig(4),
    freq0: ConfigItem                 = RegConfig(0),
    freq1: ConfigItem                 = RegConfig(0),
    freq2: ConfigItem                 = RegConfig(0),
    freq3: ConfigItem                 = RegConfig(0),
    phase0: ConfigItem                = RegConfig(0),
    phase1: ConfigItem                = RegConfig(0),
    phase2: ConfigItem                = RegConfig(0),
    phase3: ConfigItem                = RegConfig(0)
) {
  val freqs  = Seq(freq0, freq1, freq2, freq3)
  val phases = Seq(phase0, phase1, phase2, phase3)

  // assertions
  frequencyDividerRatio match {
    case ConstantConfig(value) => assert(value >= 4 && value <= 20)
    case _ => // do nothing
  }
}

/** */
// TODO: using bus interface as input for instantiation, each reg can be either fixed or configurable
class Ad9959Ctrl(config: Ad9959Config, bus: Option[BusIf], ddsBundle: Ad9959Bundle) extends Area {

  val freqs = config.freqs.zipWithIndex.map { case (value, i) =>
    value match {
      case ConstantConfig(value) => U(value, 32 bits)
      case RegConfig(defaultValue) =>
        implicit val symbolName: SymbolName = SymbolName(s"freq_$i")
        bus.get.newReg(s"freq$i").field(word(), AccessType.RW, defaultValue.toLong, s"FTW for channel $i")
    }
  }

  val phases = config.phases.zipWithIndex.map { case (value, i) =>
    value match {
      case ConstantConfig(value) => U(value, 16 bits)
      case RegConfig(defaultValue) =>
        implicit val symbolName: SymbolName = SymbolName(s"phase_$i")
        bus.get.newReg(s"phase$i").field(halfWord(), AccessType.RW, defaultValue.toLong, s"POW for channel $i")
    }
  }

  val fdr = {
    config.frequencyDividerRatio match {
      case ConstantConfig(value) => U(value, 5 bits)
      case RegConfig(defaultValue) =>
        implicit val symbolName: SymbolName = SymbolName(s"fdr")
        bus.get.newReg("fdr").field(UInt(5 bits), AccessType.RW, defaultValue.toLong, "frequency divider ratio")
    }
  }
  val fr1Reg = U(BigInt("1", 2), 1 bits) @@ getControlData(fdr) @@ U(0, 26 bits)
  fr1Reg.addTag(crossClockDomain)

  // parameters
  val waitingCycles = if (atSimTime) 100 else 1250000 // waiting 0.01s X 16 for host writing registers

  // address and length
  val CSR   = 0x00 // channel select register
  val FR1   = 0x01 // function register 1
  val CFTW0 = 0x04 // channel frequency tuning word 0
  val CPOW0 = 0x05 // channel phase offset word 0

  val csrs: Seq[UInt] = Seq(8, 4, 2, 1).map(U(_, 4 bits)).map(_ @@ U(0, 28 bits)) // CSR value for each channel

  val lengthCSR   = 8
  val lengthFR1   = 24
  val lengthCFTW0 = 32
  val lengthCPOW0 = 16

  // loop for each channel: channel select register, channel frequency tuning word 0, channel phase offset word 0
  val addrs = Seq.fill(4)(Seq(CSR, CFTW0, CPOW0)).flatten :+ FR1
  val lengths = (Seq.fill(4)(Seq(lengthCSR, lengthCFTW0, lengthCPOW0)).flatten :+ lengthFR1)
    .map(_ + 8) // + instruction length
    .map(_ + 1) // endReg value

  // constructing sclk clock domain
  val sclkCounter = CounterFreeRun(16)
  val sclkReg     = (sclkCounter >= U(8) & sclkCounter < U(10)).d() // 16 times slower than ctrlClk

  val serialIoDomain = ClockDomain(
    clock  = sclkReg,
    reset  = ClockDomain.current.reset,
    config = dasClockConfig
  )

  // do serial io in a clock domain 16 times slower than ctrlClk, meeting timing constraints easier

  val ctrlOut = ddsBundle.sdio(0) // the only signal we use to control AD9959
  val serialIoArea = new ClockingArea(serialIoDomain) {

    val values: Vec[UInt] = Vec( // TODO: pick a value from these regs may be the critical path
      ((0 until 4).flatMap(i => Seq(csrs(i), freqs(i), phases(i) @@ U(0, 16 bits))) :+ fr1Reg).map(getControlData)
    )

    // regs for output
    val cs_n, data, rst, io_update = Bool()

    // pre-connection
    ddsBundle.p.setAll()                         // set unused pins
    ddsBundle.sdio.clearAll()                    // set unused pins
    cs_n.set()                                   // set cs_n default to high
    Seq(data, rst, io_update).foreach(_.clear()) // set default to low

    // constructing change event, which is used to trigger the whole FSM
    private val freqChange  = freqs.map(getControlData).map(_.changed.d()).reduce(_ || _).d()
    private val phaseChange = phases.map(getControlData).map(_.changed.d()).reduce(_ || _).d()
    private val change      = (freqChange || phaseChange).d()

    // components
    val writeRegCounter = Counter(128)
    val dataReg         = RegInit(U(0, 40 bits))
    val endReg          = RegInit(U(0, 7 bits))

    // state machine
    val stepCount = values.length
    val ddsFsm: StateMachine = new StateMachine { // main FSM
      //
      val BOOT         = makeInstantEntry()
      val RESET        = new StateDelay(100)
      val WRITE_REGS   = Seq.fill(stepCount)(new State())
      val IO_UPDATE    = new StateDelay(4)
      val RUNNING      = new State()
      val WAITING_HOST = new StateDelay(waitingCycles)

      // state workload
      RESET.onEntry(io_update.clear())
      RESET.whenIsActive(rst.set())
      RESET.onExit(rst.clear())

      (0 until stepCount).foreach { i =>
        val state = WRITE_REGS(i)
        state.whenIsActive {
          when(writeRegCounter.value === 0) { // loading data
            dataReg := U(addrs(i), 8 bits) @@ values(i)
            endReg  := U(lengths(i), 7 bits)
            writeRegCounter.increment()
          }.elsewhen(writeRegCounter.value < endReg) { // writing
            cs_n.clear()
            data    := dataReg.msb
            dataReg := dataReg |<< 1
            writeRegCounter.increment()
          }.elsewhen(writeRegCounter.value === endReg) { //
            if (i < stepCount - 1) goto(WRITE_REGS(i + 1))
            else goto(IO_UPDATE)
            writeRegCounter.clear()
          }
        }
      }

      IO_UPDATE.whenIsActive(io_update.set())
      RUNNING.whenIsActive(io_update.set())

      // state transition
      BOOT.whenIsActive(goto(RESET))
      RESET.whenCompleted(goto(WRITE_REGS.head))
      IO_UPDATE.whenCompleted(goto(RUNNING))
      RUNNING.whenIsActive(when(change)(goto(WAITING_HOST)))
      WAITING_HOST.whenCompleted(goto(RESET))
    }
  }

  // output timing logic
  ddsBundle.rst := RegNextWhen(serialIoArea.rst, sclkReg.fall(), False)
    .addTag(crossClockDomain)
  ddsBundle.csn := RegNextWhen(serialIoArea.cs_n, sclkReg.fall(), True)
    .addTag(crossClockDomain)
  ctrlOut := RegNextWhen(serialIoArea.data, sclkReg.fall(), False)
    .addTag(crossClockDomain)
  ddsBundle.io_update := RegNextWhen(
    serialIoArea.io_update,
    sclkReg.fall(),
    False
  )
    .addTag(crossClockDomain)
  ddsBundle.sclk := sclkReg & ~ddsBundle.csn.d(8)

  // debug
  val valid = ddsBundle.sclk.rise() & ~ddsBundle.csn & ~ddsBundle.rst
  val next  = ddsBundle.csn.rise()
  valid.simPublic()
  ctrlOut.simPublic()
  next.simPublic()
  assert(~(ddsBundle.sclk.rise() & ctrlOut.changed & ~ddsBundle.rst))
}

object Ad9959Ctrl {
  def apply(config: Ad9959Config, bus: Option[BusIf], ddsBundle: Ad9959Bundle): Ad9959Ctrl =
    new Ad9959Ctrl(config, bus, ddsBundle)
}
