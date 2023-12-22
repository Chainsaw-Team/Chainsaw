package Chainsaw.project.das

import Chainsaw.DataUtil
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class DdsCtrlFsm() extends Component {

  // I/O
  val rstn                                             = in Bool ()
  val freq0Ctrl, freq1Ctrl, freqAdcCtrl, freqPulseCtrl = in(word())     // 频率控制字
  val phase0Ctrl, phase1Ctrl                           = in(halfWord()) // 相位控制字
  Seq(freq0Ctrl, freq1Ctrl, freqAdcCtrl, freqPulseCtrl, phase0Ctrl, phase1Ctrl).foreach(_.addTag(crossClockDomain))
  val ddsBundle = DdsBundle()
  Seq( // unused pins
    ddsBundle.ddsp0,
    ddsBundle.ddsp1,
    ddsBundle.ddsp2,
    ddsBundle.ddsp3
  ).foreach(_.set())

  ddsBundle.DDS_SDIO := ddsBundle.DDS_SDIO.getZero
  val dataOut = ddsBundle.DDS_SDIO(0)

  // parameters
  val waitingCycles = if (atSimTime) 100 else 12500000 // waiting 0.1s for host writing registers

  val CSR       = 0x00
  val lengthCSR = 8

  val FR1       = 0x01
  val lengthFR1 = 24

  val CFTW0       = 0x04
  val lengthCFTW0 = 32

  val CPOW0       = 0x05
  val lengthCPOW0 = 16

  val valueFR1 = ctrlClockRate match {
    case 100 => 0x940000 // 100[101]00 multiple = 5
    case 125 => 0x900000 // 100[100]00 multiple = 4
    case _   => throw new IllegalArgumentException("错误的晶振时钟频率,应当在100|125中选择")
  }

  val addrLoop   = Seq(CSR, CFTW0, CPOW0)
  val lengthLoop = Seq(lengthCSR, lengthCFTW0, lengthCPOW0) // data length

  val addrs = Seq.fill(4)(addrLoop).flatten :+ FR1
  val lengths = (Seq.fill(4)(lengthLoop).flatten :+ lengthFR1)
    .map(_ + 8) // + instruction length
    .map(_ + 1) // endReg value

  val dataRom: Vec[UInt] = Vec(
    Seq(
      U(0x80, 8 bits) @@ U(0, 24 bits),
      freq0Ctrl,
      phase0Ctrl @@ U(0, 16 bits),
      U(0x40, 8 bits) @@ U(0, 24 bits),
      freq1Ctrl,
      phase1Ctrl @@ U(0, 16 bits),
      U(0x20, 8 bits) @@ U(0, 24 bits),
      freqAdcCtrl,
      U(0, 32 bits),
      U(0x10, 8 bits) @@ U(0, 24 bits),
      freqPulseCtrl,
      U(0, 32 bits),
      U(valueFR1, 24 bits) @@ U(0, 8 bits)
    )
  )

  val stepCount = dataRom.length

  // constructing sclk clock domain
  val sclkCounter = CounterFreeRun(16)
  val sclkReg     = (sclkCounter >= U(8) & sclkCounter < U(10)).d() // 16 times slower than ctrlClk

  val serialIoDomain = ClockDomain(
    clock  = sclkReg,
    reset  = rstn,
    config = dasClockConfig
  )

  val serialIoArea = new ClockingArea(serialIoDomain) {

    // regs for output
    val ddscs, ddsData, ddsRst, ddsIoUpdate = Bool()
    ddscs.set()
    Seq(ddsData, ddsRst, ddsIoUpdate).foreach(_.clear())

    private val freqChange  = Seq(freq0Ctrl, freq1Ctrl, freqAdcCtrl, freqPulseCtrl).map(_.changed).reduce(_ || _).d()
    private val phaseChange = Seq(phase0Ctrl, phase1Ctrl).map(_.changed).reduce(_ || _).d()
    private val change      = (freqChange || phaseChange).d()

    // components
    val writeRegCounter = Counter(128)
    val dataReg         = RegInit(U(0, 40 bits))
    val endReg          = RegInit(U(0, 7 bits))

    // state machine
    val ddsFsm: StateMachine = new StateMachine {
      //
      val BOOT         = makeInstantEntry()
      val RESET        = new StateDelay(100)
      val WRITE_REGS   = Seq.fill(stepCount)(new State())
      val IO_UPDATE    = new StateDelay(4)
      val RUNNING      = new State()
      val WAITING_HOST = new StateDelay(waitingCycles) // 1 sec

      // state workload
      RESET.onEntry(ddsIoUpdate.clear())
      RESET.whenIsActive(ddsRst.set())
      RESET.onExit(ddsRst.clear())

      (0 until stepCount).foreach { i =>
        val state = WRITE_REGS(i)
        state.whenIsActive {
          when(writeRegCounter.value === 0) { // loading data
            dataReg := U(addrs(i), 8 bits) @@ dataRom(i)
            endReg  := U(lengths(i), 7 bits)
            writeRegCounter.increment()
          }.elsewhen(writeRegCounter.value < endReg) { // writing
            ddscs.clear()
            ddsData := dataReg.msb
            dataReg := dataReg |<< 1
            writeRegCounter.increment()
          }.elsewhen(writeRegCounter.value === endReg) { //
            if (i < stepCount - 1) goto(WRITE_REGS(i + 1))
            else goto(IO_UPDATE)
            writeRegCounter.clear()
          }
        }
      }

      IO_UPDATE.whenIsActive(ddsIoUpdate.set())
      RUNNING.whenIsActive(ddsIoUpdate.set())

      // state transition
      BOOT.whenIsActive(goto(RESET))
      RESET.whenCompleted(goto(WRITE_REGS.head))
      IO_UPDATE.whenCompleted(goto(RUNNING))
      RUNNING.whenIsActive(when(change)(goto(WAITING_HOST)))
      WAITING_HOST.whenCompleted(goto(RESET))
    }
  }

  // output
  ddsBundle.ddsrst := RegNextWhen(serialIoArea.ddsRst, sclkReg.fall(), False)
    .addTag(crossClockDomain)
  ddsBundle.ddscs := RegNextWhen(serialIoArea.ddscs, sclkReg.fall(), True)
    .addTag(crossClockDomain)
  dataOut := RegNextWhen(serialIoArea.ddsData, sclkReg.fall(), False)
    .addTag(crossClockDomain)
  ddsBundle.ddsioupdate := RegNextWhen(
    serialIoArea.ddsIoUpdate,
    sclkReg.fall(),
    False
  )
    .addTag(crossClockDomain)
  ddsBundle.ddsclk := sclkReg & ~ddsBundle.ddscs.d(8)

  // debug
  val valid = ddsBundle.ddsclk.rise() & ~ddsBundle.ddscs & ~ddsBundle.ddsrst
  val next  = ddsBundle.ddscs.rise()
  valid.simPublic()
  dataOut.simPublic()
  next.simPublic()
  assert(~(ddsBundle.ddsclk.rise() & dataOut.changed & ~ddsBundle.ddsrst))
}
