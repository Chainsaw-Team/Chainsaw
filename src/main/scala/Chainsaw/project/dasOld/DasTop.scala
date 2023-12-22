package Chainsaw.project.das

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.{CHeaderGenerator, HtmlGenerator}

import java.io.File
import scala.language.postfixOps

/** Das系统采集卡Top Module,实现以下功能:
  *   1. 数据采集和跨时钟域串并转换 2. 脉冲生成,以及根据脉冲生成时间插入帧头 3. PCIe逻辑,包括数据上传通道和控制寄存器
  */
case class DasTop() extends Component with DasBoard {

  setDefinitionName("Das")

  this.getAllIo.filter(_.isOutput).foreach(_.assignDontCare())

  val dataClk = pulseGenClkFromDds

  // IP和时钟连线
  // ADC数据输入模块
  private val signalAcq = LVDSDEBUG() // 数据采集模块,输出1/4采样率时钟
  signalAcq.Adc_clk   := dataClkFromAdc
  signalAcq.RstIn     := rstn
  signalAcq.adcBundle <> adcBundle

  // 1/4 -> 1/2数据转换+多相分解,输出1/2采样率时钟
  private val polyPhase = DasPolyPhase()
  polyPhase.clk62_5 := signalAcq.clkout
  polyPhase.rstn    := rstn
  polyPhase.clk125  := dataClk
  polyPhase.dataIn  <> signalAcq.adcData

  // PCIe模块
  val xillybus = XillybusWrapper(dasDevices)
  xillybus.ctrlClk := ctrlClkFromOsc
  xillybus.dataClk := dataClk
  xillybus.pcie    <> pcieBundle

  // 时钟域声明
  private def getClockDomain(clk: Bool, freq: Option[FixedFrequency] = None) =
    freq match {
      case Some(value) =>
        ClockDomain(
          clock     = clk,
          reset     = rstn,
          config    = dasClockConfig,
          frequency = value
        )
      case None =>
        ClockDomain(clock = clk, reset = rstn, config = dasClockConfig)
    }

  private val ctrlDomain =
    getClockDomain(ctrlClkFromOsc, Some(FixedFrequency(100 MHz))) // 全局控制时钟域
  private val pulseGenDomain = getClockDomain(
    pulseGenClkFromDds
  ) // 脉冲发送|数据处理时钟域

  val ctrlArea = new ClockingArea(ctrlDomain) { // 全局控制逻辑

    //
    val busIf = XillybusIf(xillybus.ctrlOut)

    // 寄存器控制单元
    val controller = DasController(busIf)

    // DDS控制单元
    val ddsCtrl = DdsCtrlFsm()
    ddsCtrl.rstn          := rstn
    ddsCtrl.freq0Ctrl     := controller.freq0Value
    ddsCtrl.freq1Ctrl     := controller.freq1Value
    ddsCtrl.freqAdcCtrl   := controller.adcFreqValue
    ddsCtrl.freqPulseCtrl := controller.pulseFreqValue
    ddsCtrl.phase0Ctrl    := controller.phase0Value
    ddsCtrl.phase1Ctrl    := controller.phase1Value
    ddsBundle             <> ddsCtrl.ddsBundle

  }

  // 在全局控制时钟域与其它时钟域间传递寄存器值
  private def getControlData[T <: Data](ctrl: T) = {
    ctrl.addTag(crossClockDomain)
    ctrl.d(3)
  }

  // 在脉冲发生时钟域与信号处理时钟域间传递脉冲信号
  private val pulseGenArea = new ClockingArea(pulseGenDomain) {
    // 脉冲发送功能
    val pulseGen = DasPulseGen()
    pulseGen.pulseGenCtrl := getControlData(ctrlArea.controller.pulseGenCtrl)
    pulseBundle           := pulseGen.pulseBundle
    gain                  := pulseGen.gainOut
    val pulseRise = pulseGen.pulseOnFixed.rise() // 脉冲上升沿信号

    // 数据采集/解调功能
    val useChannel0 = getControlData(ctrlArea.controller.useChannel0)

    val dasDatapath = DataPath(dataWidth)
    dasDatapath.dataIn(0) := Mux(useChannel0, polyPhase.dataOut.adc0X0S, polyPhase.dataOut.adc1X0S).asUInt
    dasDatapath.dataIn(1) := Mux(useChannel0, polyPhase.dataOut.adc0X1S, polyPhase.dataOut.adc1X1S).asUInt
    dasDatapath.pulseIn   := pulseRise
    dasDatapath.selfTest  := getControlData(ctrlArea.controller.doSelfTest)
    dasDatapath.header    := B(HEADER, 32 bits)

    // GPS解码

    val gpsDecode = GpsDecode()
    gpsDecode.rstn      := rstn
    gpsBundle           <> gpsDecode.gpsBundle
    dasDatapath.gpsInfo := gpsDecode.infoOut.asBits
  }

  // 数据上传
  private val acqChannel = xillybus.getWriteInterfaceByName("acq") // 数据上传通道
  acqChannel.wrreq := pulseGenArea.dasDatapath.dataOut.valid
  acqChannel.data  := pulseGenArea.dasDatapath.dataOut.payload

  // debug
  /** 增加一个名为XXX_counter_XXMHz的计数信号,这个信号可以被SignalTap监测,以判断相应时钟源的运行状况
    *
    * @param clk
    *   时钟源
    * @param clk_name
    *   时钟名称
    */
  private def addClockCounter(clk: Bool, clk_name: String): Unit = {
    val domain = getClockDomain(clk)
    new ClockingArea(domain) {
      val counter = CounterFreeRun(1024)
      counter.value.setName(s"${clk_name}_counter")
      out(counter.value)
    }
  }

  addClockCounter(pcieBundle.refclk, "pcie_ref_clk")
  addClockCounter(dataClkFromAdc, "adc_clk")
  addClockCounter(pulseGenClkFromDds, "data_clk")
  addClockCounter(ctrlClkFromOsc, "ctrl_clk")

  // Osc
  // 通过PulseOut2,可以将信号通过采集卡的最后一个SMA接口输出并通过示波器观察
  //  pulseBundle.PulseOut2.allowOverride
  //  pulseBundle.PulseOut2 := ddsClkForAdc

  // doc & header generation
  ctrlArea.busIf.accept(HtmlGenerator("DasRegs", "DAS SYSTEM"))
  ctrlArea.busIf.accept(CHeaderGenerator("regIf", "DAS"))
  ctrlArea.busIf.accept(PythonHeaderGenerator("regIf", "DAS"))
}

object DasTop {

  def main(args: Array[String]): Unit = {

    if (args.nonEmpty) {
      ctrlClockRate = args(0).toInt
      dataWidth     = args(1).toInt
    }

    // 生成RTL代码,python头文件和寄存器文档
    val sourceDir = new File("src/main/resources/das/DasBuild")
    SpinalConfig(
      netlistFileName              = "Das.v",
      targetDirectory              = s"$sourceDir/src",
      defaultConfigForClockDomains = dasClockConfig
    ).generateVerilog(DasTop())

    println(s"DasTop生成成功,晶振频率=${ctrlClockRate}MHz,数据位宽${dataWidth}bits")
  }

}
