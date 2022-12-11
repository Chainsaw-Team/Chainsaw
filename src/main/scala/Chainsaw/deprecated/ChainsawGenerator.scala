package Chainsaw.deprecated

import Chainsaw.xilinx.{VivadoUtil, VivadoUtilRequirement}
import Chainsaw.{FrameBased, FrameFormat, ImplMode, atSimTime, logger, naiveSet}
import spinal.core.sim.SpinalSimBackendSel
import spinal.core.sim.SpinalSimBackendSel.VERILATOR
import spinal.core.{Bits, Data, HertzNumber, Vec, assert}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

trait ChainsawGenerator {

  def name: String

  /** -------- golden model --------
   *
   */
  def impl(dataIn: Seq[Any]): Seq[Any] // golden model

  val implMode: ImplMode = FrameBased

  val metric: ChainsawMetric = ChainsawMetric.defaultMetric

  val simBackEnd: SpinalSimBackendSel = VERILATOR

  def generateTestCases: Seq[Any] = null

  /** -------- size information --------
   *
   */
  def inputTypes: Seq[NumericType]

  def outputTypes: Seq[NumericType]

  /** -------- timing information --------
   *
   */
  def inputFormat: FrameFormat

  def outputFormat: FrameFormat

  val inputTimes: Option[Seq[Int]] = None // when this is empty, inputs are aligned
  val outputTimes: Option[Seq[Int]] = None

  def latency: Int // defined as the latency from the head of inputs to the head of outputs

  def offset: Int = 0

  /** -------- performance information --------
   *
   */
  def utilEstimation: VivadoUtil = VivadoUtilRequirement()

  def fmaxEstimation: HertzNumber = 600 MHz

  /** -------- implementations --------
   *
   */
  def implH: ChainsawModule // core module, that is, the datapath

  def implNaiveH: Option[ChainsawModule] = None // naive RTL implementation for simulation & top-down design, optional

  def implPass: ChainsawModule = new ChainsawModule(this) {
    dataIn.foreach(_.addAttribute("dont_touch", "yes"))
    dataOut.foreach(_.assignDontCare())
    dataOut.foreach(_.addAttribute("dont_touch", "yes"))
  }

  def doSelfTest(): ChainsawTestReport = ChainsawTest(s"test_$name", this, generateTestCases).doTest()

  def setAsNaive(): Unit = naiveSet += this.getClass.getSimpleName

  def useNaive: Boolean = naiveSet.contains(this.getClass.getSimpleName)

  def getImplH: ChainsawModule = {
    doDrc()
    if (useNaive && !atSimTime) implPass
    else if (useNaive) implNaiveH.getOrElse(implH)
    else implH
  }

  /** -------- utils --------
   *
   */
  // when a module need no control, you can use it as a function
  def asFunc: Seq[Bits] => Seq[Bits] = (dataIn: Seq[Bits]) => {
    if (!needNoControl) logger.warn(s"you're using $name as a function while it may need control")
    val core = implH
    core.setFreeRun()
    core.dataIn := Vec(dataIn)
    core.dataOut
  }

  def process[T <: Data](dataIn: Seq[T]) = {
    val core = implH
    core.setFreeRun()
    core.dataIn := Vec(dataIn.map(_.asBits))
    core.dataOut
  }

  def doDrc(): Unit = {
    assert(inputFormat.period == outputFormat.period, s"input: ${inputFormat.period}, output: ${outputFormat.period}")
    assert(inputFormat.portSize == inputTypes.length, s"inputFrame port number: ${inputFormat.portSize}, actual input port number: ${inputTypes.length}")
    assert(outputFormat.portSize == outputTypes.length, s"outputFrame port number: ${outputFormat.portSize}, actual output port number: ${outputTypes.length}")

    assert(actualInTimes.head == 0, s"${actualInTimes.mkString(" ")}")
    assert(actualInTimes.length == inputWidths.length)
    assert(actualInTimes.forall(_ >= 0))
    assert(actualOutTimes.head == 0)
    assert(actualOutTimes.length == outputWidths.length)
    assert(actualOutTimes.forall(_ >= 0))

    assert(
      latency >= 0,
      "invalid generator with negative latency, " +
        "do you forgot to invoke GraphDone at the end of Dag construction?"
    )
  }

  final def inputWidths: Seq[Int] = inputTypes.map(_.bitWidth)

  final def outputWidths: Seq[Int] = outputTypes.map(_.bitWidth)

  def sizeIn: Int = inputWidths.length

  def sizeOut: Int = outputWidths.length

  def needNoControl: Boolean = inputFormat.period == 1 && outputFormat.period == 1

  def inputNoControl: FrameFormat = FrameFormat(Seq(0 until sizeIn))

  def outputNoControl: FrameFormat = FrameFormat(Seq(0 until sizeOut))

  def actualInTimes: Seq[Int] = inputTimes.getOrElse(inputTypes.map(_ => 0))

  def actualOutTimes: Seq[Int] = outputTimes.getOrElse(outputTypes.map(_ => 0))

  def period = {
    require(inputFormat.period == outputFormat.period)
    inputFormat.period
  }
}
