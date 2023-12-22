package Chainsaw.project.das

import Chainsaw._
import Chainsaw.memory._
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._

import scala.language.postfixOps

// TODO: 实装AGC响应时间补偿

/** 这个模块实现对端口5,6脉冲和放大器增益的周期性控制,工作在脉冲发生时钟域
  */


case class DasPulseGen() extends Component {

  // 控制信号
  val pulseGenCtrl = in(PulseGenCtrlBundle())
  import pulseGenCtrl._

  // 输出
  val pulseBundle  = PulseBundle()
  val gainOut      = out UInt (6 bits)
  val pulseOnFixed = out Bool ()

  // 脉冲发生功能
  private val pulseCounter = DynamicCounterFreeRun(pulsePoints) // 用于产生脉冲的计数器
  // FIXME: 直接从比较器产生的逻辑有毛刺
  // pulseCounter.value is a registered output
  private val pulseOn = (pulseCounter.value.takeLow(16).asUInt < pulseWidth).d() // 所有脉冲输出的源头

  def getDynamicDelayed(signal: Bool, delay: UInt): Bool = {
    val delayModule = DynamicDelayModule(255, 1)
    delayModule.payloadIn := signal.asBits
    delayModule.delay     := delay
    delayModule.payloadOut.asBool
  }

  private val pulseOnDelayed = getDynamicDelayed(pulseOn, pulseDelayPoints) // 根据控制参数对在两个脉冲间插入延迟

  when(pulseOn)(pulseBundle.Pulseout0         := True)   //
  when(pulseOn)(pulseBundle.Pulseout0N        := False)
  when(pulseOnDelayed)(pulseBundle.Pulseout1  := True)   //
  when(pulseOnDelayed)(pulseBundle.Pulseout1N := False)
  pulseBundle.PulseOut2                       := pulseOn // pulse2可以直接在示波器上看到

  // 为DataPath提供帧头
  pulseOnFixed := getDynamicDelayed(pulseOn, pulseFixPoints) // 对从发射脉冲到收到反射之间的时间差进行补正

  // AGC功能,通过嵌套计数器构造随距离增大的增益补偿
  private val gainCounter      = DynamicCounterFreeRun(agcPoints)
  private val gainValueCounter = Counter(64, inc = gainCounter.willOverflow)
  // 与脉冲上升沿同步
  when(pulseCounter.willOverflow)(gainCounter.clear())
  when(pulseCounter.willOverflow)(gainValueCounter.clear())

  private val dynamicGain = (gainValue +| gainValueCounter.value).d() //动态增益 = 静态增益+距离补偿,在63达到饱和
  gainOut := U(63) - dynamicGain // 实际控制字,数值越小,增益越大


  // debug
  pulseOn.setName("PulseOn")
  pulseOnFixed.setName("pulseOnFixed")
  pulseOnDelayed.setName("pulseOnDelayed")
  val pulseRise = pulseOn.rise(False)
  pulseRise.simPublic()
}
