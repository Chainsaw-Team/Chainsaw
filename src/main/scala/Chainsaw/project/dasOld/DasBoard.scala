package Chainsaw.project.das

import Chainsaw.edaFlow.boards.PcieIntel
import spinal.core._
import spinal.lib.slave

import scala.language.postfixOps

/** 描述采集卡FPGA与外部的接口,通过继承这个trait,Component可以获得DAS信号采集卡的所有引脚信息
  */
trait DasBoard { // Top模块的引脚,各个需要上板的设计从该模块上进行继承

  // 时钟域信号
  val ctrlClkFromOsc                     = in Bool () // 晶振产生的100/125MHz全局时钟
  val pulseGenClkFromDds, dataClkFromAdc = in Bool () // DDS时钟输出
  val rstn                               = in Bool ()

  lazy val ddsBundle   = DdsBundle()         // DDS控制输出
  lazy val adcBundle   = Adc250()            // ADC数据输入
  val gain             = out UInt (6 bits)   // 放大器增益控制
  lazy val pulseBundle = PulseBundle()       // 脉冲输出
  lazy val pcieBundle  = slave(PcieIntel(4)) // PCIe接口
  lazy val gpsBundle   = GpsBundle()         // GPS模块间的接口

  // 通过setName确保生成代码的信号名
  ctrlClkFromOsc.setName("BUF_CLK")
  rstn.setName("RstIn")
  dataClkFromAdc.setName("Adc_clk")
  pulseGenClkFromDds.setName("DDS_clk")
  gain.setName("VGA_B")

}
