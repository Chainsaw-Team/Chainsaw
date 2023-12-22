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

/** Represents a bundle for controlling the AD9959 device.
  *
  * @param csn
  *   The chip select signal, active low.
  * @param sclk
  *   The serial clock signal.
  * @param rst
  *   The reset signal.
  * @param io_update
  *   The I/O update signal.
  * @param p
  * @param sdio
  *   A 4-bit I/O interface for serial data to/from AD9959
  */
case class Ad9959Bundle() extends Bundle {
  val csn, sclk, rst, io_update = Bool()
  val p                         = Bits(4 bits)
  val sdio                      = Bits(4 bits) // 存在高阻态的信号
  this.setName("AD9959")
}

case class PulseBundle() extends Bundle {
  val sma0p, sma0n, sma1p, sma1n = Bool()
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
