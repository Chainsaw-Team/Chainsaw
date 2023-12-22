package Chainsaw.project.das

import Chainsaw._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** 实现跨时钟域的并串转换和多相分解
 */
case class DasPolyPhase() extends Component {
  val clk125, clk62_5, rstn = in Bool() // 62.5MHz/125 MHz时钟和异步复位输入
  val dataIn = in(Adc62_5())
  val dataOut = out(Adc125())

  // ADC数据从62.5MHz时钟域到125MHz时钟域
  private val adcData0 = Vec(dataIn.DOUTD, dataIn.DOUTC, dataIn.DOUTB, dataIn.DOUTA)
  val adcData1 = Vec(dataIn.DOUTBD, dataIn.DOUTBC, dataIn.DOUTBB, dataIn.DOUTBA)

  // 多相分解
  val channels: Seq[Vec[Bits]] = adcData0.groupByChannel(2) ++ adcData1.groupByChannel(2)

  // 例化串并转换模块
  def getSerial(vec: Vec[Bits]): Bits = {
    val p2s = P2SCC(14, 2)
    p2s.clkIn := clk62_5
    p2s.rstn := rstn
    p2s.clkOut := clk125
    p2s.dataIns := vec
    p2s.dataOut
  }

  dataOut.flatten.zip(channels).foreach { case (out, channel) => out := getSerial(channel) }
}


// TODO: 移动到Chainsaw core

/** 跨时钟域的并串转换模块,从慢时钟域到快时钟域
 *
 * @param factor
 * 并串转换倍数,也是时钟域频率倍数
 */
case class P2SCC(width: Int, factor: Int) extends Component {

  // clk for parallel in and serial out
  val clkIn, clkOut, rstn = in Bool()
  val dataIns = in Vec(Bits(width bits), factor) // parallel in, dataIns(0) should be the first element in serial out
  val dataOut = out Bits (width bits) // serial out

  // 定义时钟域
  val domainSlow = ClockDomain(clock = clkIn, reset = rstn, config = dasClockConfig)
  val domainFast = ClockDomain(clock = clkOut, reset = rstn, config = dasClockConfig)

  // 通过异步FIFO传送数据
  val fifo = StreamFifoCC(Bits(width * factor bits), 64, domainSlow, domainFast)

  new ClockingArea(domainSlow) {
    fifo.io.push.valid := True // 实时系统总是产生有效信号
    fifo.io.push.payload := dataIns.reduce(_ ## _)
  }

  new ClockingArea(domainFast) { // 在快时钟域进行并串转换
    val reqCounter = CounterFreeRun(factor) // controlled by a counter
    fifo.io.pop.ready := reqCounter.willOverflow // 周期性地取出数据
    val parallel = RegNextWhen(fifo.io.pop.payload, reqCounter.willOverflow)
    val qs = parallel.subdivideIn(width bits).reverse // 将数据分发到各个周期
    switch(reqCounter.value) {
      qs.zipWithIndex.foreach { case (q, i) => is(i)(dataOut := q.d(i)) }
    }
  }
}
