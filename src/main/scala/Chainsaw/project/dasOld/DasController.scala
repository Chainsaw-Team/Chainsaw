package Chainsaw.project.das

import spinal.core._
import spinal.lib.bus.regif._

import scala.language.postfixOps

// TODO: 将寄存器升级到双向读写

/** 这个模块实现基于RegIf和Xillybus的控制寄存器,工作在控制时钟域
  */
case class DasController(ctrlIf: BusIf) extends Area {

  //  DDS频率/相位控制字,修改这些状态时,需要重置DDS
  val word250M           = 1.toLong << 31
  val mhz_number         = word250M / 250
  def freqWord(mhz: Int) = mhz * mhz_number // 计算频率控制字

  private val port5     = ctrlIf.newReg("port5频率控制")
  val freq0Value        = port5.field(word, AccessType.RW, freqWord(100).toLong, "端口5频率 默认100MHz")
  private val port6     = ctrlIf.newReg("port6频率控制")
  val freq1Value        = port6.field(word, AccessType.RW, freqWord(80).toLong, "端口6频率 默认80MHz")
  private val adcFreq   = ctrlIf.newReg("ADC采样时钟控制")
  val adcFreqValue      = adcFreq.field(word, AccessType.RW, word250M, "ADC采样时钟频率,默认250MHz")
  private val pulseFreq = ctrlIf.newReg("脉冲发生域时钟,频率为采样频率的1/2")
  val pulseFreqValue    = pulseFreq.field(word, AccessType.RW, word250M / 2, "脉冲发生|数据处理时钟频率,为ADC采样率的1/2")

  private val port5Phase = ctrlIf.newReg("port5phase")
  val phase0Value        = port5Phase.field(halfWord, AccessType.RW, 0, "端口5相位,初始值0°")
  private val port6Phase = ctrlIf.newReg("port6phase")
  val phase1Value        = port6Phase.field(halfWord, AccessType.RW, (1 << 13).toLong, "端口6相位,初始值180°")

  // 增益控制字
  private val gainCtrl = ctrlIf.newReg("电放静态增益控制")
  val gainValue = gainCtrl.field(
    HardType(UInt(6 bits)),
    AccessType.RW,
    37,
    "基准增益值: 28为最小,63为最大,28 -> 35 -> -9dB,63 -> 0 -> 26dB,默认值为0dB"
  )
  private val agcCtrl = ctrlIf.newReg("AGC控制")
  val agcPoints       = agcCtrl.field(word, AccessType.RW, 125000 / 2, "动态增益梯度,每n个周期+1,默认不开启(与脉冲点数相同)\n")

  // 脉冲发送控制字
  private val pulseCtrl0 = ctrlIf.newReg("脉冲重频控制")
  private val pulseCtrl1 = ctrlIf.newReg("脉宽控制")
  private val pulseCtrl2 = ctrlIf.newReg("脉冲时延控制")
  val pulsePoints        = pulseCtrl0.field(word, AccessType.RW, 125000 / 2, "一个脉冲对应的周期数,默认值对应于250MHz,2kHz")
  val pulseWidth         = pulseCtrl1.field(halfWord, AccessType.RW, 13, "脉宽对应的周期数,默认值对应于250MHz,104ns")
  val pulseDelayPoints   = pulseCtrl2.field(byte, AccessType.RW, 0, "双脉冲时间差对应的周期数,默认值对应于0ns")
  val pulseFixPoints     = pulseCtrl2.field(byte, AccessType.RW, 0, "脉冲与帧头的时间差对应的周期数,默认值对应于0ns")

  // 采集控制字
  private val sourceReg = ctrlIf.newReg("信号源控制")
  val useChannel0       = sourceReg.field(Bool(), AccessType.RW, 0, "数据源控制,为0时采用端口1,2(上方端口);为1时采用端口3,4(下方端口)")
  private val selfTest  = ctrlIf.newReg("自测配置")
  val doSelfTest        = selfTest.field(HardType(Bool), AccessType.RW, 0, "使用自测模式时,产生周期性自增数据,用于检查数据完整性,默认不采用")

  // 组织bundle
  val pulseGenCtrl = PulseGenCtrlBundle()
  pulseGenCtrl.pulsePoints      := pulsePoints
  pulseGenCtrl.pulseWidth       := pulseWidth
  pulseGenCtrl.pulseDelayPoints := pulseDelayPoints
  pulseGenCtrl.pulseFixPoints   := pulseFixPoints
  pulseGenCtrl.gainValue        := gainValue
  pulseGenCtrl.agcPoints        := agcPoints
  pulseGenCtrl.agcLatency       := U(0)
}
