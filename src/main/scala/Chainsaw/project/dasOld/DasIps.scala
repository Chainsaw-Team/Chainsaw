// DAS项目中使用的RTL blackbox,其RTL代码存在于src/main/resources/das/DasBuild

package Chainsaw.project.das

import spinal.core._

import scala.language.postfixOps

// TODO: 将下面的模块改为通过SpinalHDL实现

/** 匡总编写的ADC数据采集模块
 *
 */
case class LVDSDEBUG() extends BlackBox {
  val Adc_clk, RstIn = in Bool()
  val adcBundle = in(Adc250())
  val adcData = out(Adc62_5())
  val clkout = out Bool()
}