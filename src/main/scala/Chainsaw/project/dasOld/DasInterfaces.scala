package Chainsaw.project.das

import spinal.core._

/** GPS芯片与FPGA的接口
 */
case class GpsBundle() extends Bundle {
  val PPS, FPGA_RXD_232, IRIG_B, LEDLOCK_N, LED_PV = in Bool ()
  val FPGA_TXD_232                                 = out Bool ()
  LEDLOCK_N.setName("LEDLOCK")
  this.setName("")
}

/** ADC芯片与FPGA的接口
 */
case class Adc250() extends Bundle {
  val AdcinA, AdcinB = in Bits (7 bits) // 名字与引脚匹配
  this.setName("")
}

/** DDS芯片与FPGA的接口
 */
case class DdsBundle() extends Bundle { // 用于控制DDS芯片
  val ddscs, ddsclk, ddsrst, ddsp0, ddsp1, ddsp2, ddsp3, ddsioupdate = out Bool ()
  val DDS_SDIO                                                       = out(Bits(4 bits)) // 存在高阻态的信号
  this.setName("")
}

/** 脉冲输出与FPGA的接口
 */
case class PulseBundle() extends Bundle {
  //  val Pulseout1, Pulseout1N = out Bool ()           // 6
  //  val Pulseout0, Pulseout0N = out Bool ()           // 5
  val Pulseout0, Pulseout0N = inout(Analog(Bool())) // 5
  val Pulseout1, Pulseout1N = inout(Analog(Bool())) // 6
  val PulseOut2             = out Bool ()           // 7
  this.setName("")
}

// 内部接口

/** ADC数据从LVDS离开的接口,频率为1/4采样率
 */
case class Adc62_5() extends Bundle { // 信号采集模块输出的ADC数据
  val DOUTA, DOUTB, DOUTC, DOUTD, DOUTBA, DOUTBB, DOUTBC, DOUTBD = Bits(14 bits)
  this.setName("")
}

/** ADC进入数据处理时钟域的接口,频率为1/2采样率.空间上,adc0对应于上面的两个端口,adc1对应于下面的两个端口;时间上,0S的数据在时域上在1S之前
 */
case class Adc125() extends Bundle { // 送入信号处理时钟域的ADC数据
  val adc0X0S, adc0X1S, adc1X0S, adc1X1S = Bits(14 bits)
  this.setName("")
}

// 脉冲发送控制接口
case class PulseGenCtrlBundle() extends Bundle {
  val pulsePoints      = word()
  val pulseWidth       = halfWord()
  val pulseDelayPoints = byte()
  val pulseFixPoints   = byte()
  val gainValue        = UInt(6 bits)
  val agcPoints        = word()
  val agcLatency       = byte()
}

