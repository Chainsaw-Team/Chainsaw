package Chainsaw.project.das

import Chainsaw.DataUtil
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class GpsInfo() extends Bundle {
  val hour   = UInt(6 bits)
  val minute = UInt(8 bits)
  val second = UInt(8 bits)
  val ms     = UInt(10 bits)
}

case class GpsDecode() extends Component {

  // TODO: 在解码完成之间,通过自增逻辑更新时分秒,以避免产生错误的时间戳

  // IO
  val rstn      = in Bool ()
  val gpsBundle = GpsBundle() // board IO

  val infoOut  = out(GpsInfo())
  gpsBundle.FPGA_TXD_232.assignDontCare() // unused IO

  // parameter
  def get_cycle(ms: Int) = ms.toInt
  val HEAD_WIDTH         = get_cycle(8)
  val SET_WIDTH          = get_cycle(5)
  val CLEAR_WIDTH        = get_cycle(2)

  // constructing clkForGps clock domain
  val RATIO          = if (atSimTime) 125 else 125000
  val counterViewGps = CounterFreeRun(RATIO) // 对125M时钟进行分频,得到1KHz时钟
  val clkForGps      = (counterViewGps >= U(RATIO / 2) & counterViewGps < U(RATIO)).d()
  clkForGps.simPublic()

  val myDomain = ClockDomain(
    clock  = clkForGps,
    reset  = rstn,
    config = dasClockConfig
  )

  val myArea = new ClockingArea(myDomain) {

    val infoOutBuffer = RegInit(UInt(28 bits).getZero)
    val bitCounter    = Counter(28)
    val pulseCounter  = Counter(get_cycle(10))
    val msCounter     = CounterFreeRun(1000)
    val done          = Bool()

    when(gpsBundle.PPS.rise())(msCounter.clear())

    when(gpsBundle.IRIG_B)(pulseCounter.increment())
      .elsewhen(gpsBundle.IRIG_B.fall())(pulseCounter.clear())

    val lastPulseWidth     = RegInit(UInt(4 bits).getZero)
    val lastLastPulseWidth = RegNextWhen(lastPulseWidth, gpsBundle.IRIG_B.fall())
    when(gpsBundle.IRIG_B.fall())(lastPulseWidth := pulseCounter.value)

    val is_head  = lastPulseWidth === U(HEAD_WIDTH) & lastLastPulseWidth === U(HEAD_WIDTH)
    val is_set   = lastPulseWidth === U(SET_WIDTH)
    val is_clear = lastPulseWidth === U(CLEAR_WIDTH)

    // default assignment
    done.clear()

    val GpsFsm = new StateMachine {
      val BOOT   = makeInstantEntry()
      val IDLE   = new State()
      val DECODE = new State()

      // state transition logic
      BOOT.whenIsActive(goto(IDLE))
      IDLE.whenIsActive(when(is_head & !gpsBundle.LEDLOCK_N)(goto(DECODE)))
      DECODE.whenIsActive(when(bitCounter.value === U(27))(goto(IDLE)))

      // state workload
      IDLE.onEntry(bitCounter.clear())
      DECODE.whenIsActive {
        // bit counter control
        when(gpsBundle.IRIG_B.fall().d() & ~is_head)(bitCounter.increment())
        when(gpsBundle.IRIG_B.fall().d()) {
          when(~is_head) {
            when(is_set)(infoOutBuffer(bitCounter.value) := True)
              .elsewhen(is_clear)(infoOutBuffer(bitCounter.value) := False)
          }
        }
      }
      DECODE.onExit(done.set())
    }
  }

  val infoOutReg = RegInit(GpsInfo().getZero).addTag(crossClockDomain)
  when(myArea.done) {
    infoOutReg.second := (myArea.infoOutBuffer(8 downto 5) ## myArea.infoOutBuffer(3 downto 0)).asUInt
    infoOutReg.minute := (myArea.infoOutBuffer(17 downto 14) ## myArea.infoOutBuffer(12 downto 9)).asUInt
    infoOutReg.hour   := (myArea.infoOutBuffer(25 downto 24) ## myArea.infoOutBuffer(22 downto 19)).asUInt
  }
  infoOutReg.ms := myArea.msCounter.value
  infoOut := infoOutReg.d(3)
}
