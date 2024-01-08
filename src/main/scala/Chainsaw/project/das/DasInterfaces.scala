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
  val adc_a, adc_b = in Bits (7 bits)
  this.setName("")
}

/** Represents a bundle for controlling the AD9959 device.
  *
  * @param csn
  *   The chip select signal, active low.
  * @param sclk
  *   The clock output for controlling AD9959.
  * @param rst
  *   The reset signal.
  * @param io_update
  *   The I/O update signal.
  * @param p
  *   Unused
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

// TODO: optimize naming
/** ADC数据从LVDS离开的接口,频率为1/4采样率
  */
case class Adc62_5() extends Bundle { // 信号采集模块输出的ADC数据
  val DOUTA, DOUTB, DOUTC, DOUTD, DOUTBA, DOUTBB, DOUTBC, DOUTBD = Bits(14 bits)
  this.setName("")
}
