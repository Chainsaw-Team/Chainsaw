package Chainsaw

import spinal.core._

import scala.language.postfixOps

sealed trait ImplMode

object Comb extends ImplMode

object StateMachine extends ImplMode

import xilinx._

trait ChainsawGenerator {

  def name: String

  /** --------
   * golden model
   * -------- */
  def impl(dataIn: Seq[Any]): Seq[Any] // golden model

  val implMode: ImplMode = Comb

  val metric: ChainsawMetric = ChainsawMetric.defaultMetric

  /** --------
   * size information
   * -------- */
  var inputTypes: Seq[NumericType]
  var outputTypes: Seq[NumericType]

  /** --------
   * timing information
   * -------- */
  var inputFormat: FrameFormat
  var outputFormat: FrameFormat
  val inputTimes: Option[Seq[Int]] = None // when this is empty, inputs are aligned
  val outputTimes: Option[Seq[Int]] = None
  var latency: Int // defined as the latency from the head of inputs to the head of outputs

  /** --------
   * performance information
   * -------- */
  var utilEstimation: VivadoUtil = VivadoUtilRequirement()
  var fmaxEstimation: HertzNumber = 600 MHz

  /** --------
   * implementations
   * -------- */
  def implH: ChainsawModule // core module, that is, the datapath

  def implNaiveH: Option[ChainsawModule] = None // naive RTL implementation for simulation & top-down design

  def implPass: ChainsawModule = new ChainsawModule(this) {
    dataIn.foreach(_.addAttribute("dont_touch", "yes"))
    dataOut.foreach(_.assignDontCare())
    dataOut.foreach(_.addAttribute("dont_touch", "yes"))
  }

  def setAsNaive(): Unit = naiveSet += this.getClass.getSimpleName

  def useNaive: Boolean = naiveSet.contains(this.getClass.getSimpleName)

  def getImplH: ChainsawModule = {
    doDrc()
    if (useNaive && !atSimTime) implPass
    else if (useNaive) implNaiveH.getOrElse(implH)
    else implH
  }

  /** --------
   * utils
   * -------- */
  // when a module need no control, you can use it as a function
  def asFunc: Seq[Bits] => Seq[Bits] = (dataIn: Seq[Bits]) => {
    require(needNoControl)
    val core = implH
    core.setFreeRun()
    core.dataIn := Vec(dataIn)
    core.dataOut
  }

  def doDrc(): Unit = {
    assert(inputFormat.period == outputFormat.period, s"input: ${inputFormat.period}, output: ${outputFormat.period}")

    assert(actualInTimes.head == 0, s"${actualInTimes.mkString(" ")}")
    assert(actualInTimes.length == inputWidths.length)
    assert(actualInTimes.forall(_ >= 0))
    assert(actualOutTimes.head == 0)
    assert(actualOutTimes.length == outputWidths.length)
    assert(actualOutTimes.forall(_ >= 0))

    assert(latency >= 0, "invalid generator with negative latency, " +
      "do you forgot to invoke GraphDone at the end of Dag construction?")
  }

  final def inputWidths = inputTypes.map(_.bitWidth)

  final def outputWidths = outputTypes.map(_.bitWidth)

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

  // TODO: this should be implemented in Dag
  def ->(that: ChainsawGenerator) = {
    require(that.inputFormat.portSize == this.outputFormat.portSize, s"out ${this.outputFormat.portSize} -> in ${that.inputFormat.portSize}")
    //    require(that.inputFormat.period == this.outputFormat.period, s"prev period ${this.outputFormat.period} -> next period ${that.outputFormat.period}")
    val old = this
    new ChainsawGenerator {
      override def name = old.name + "_" + that.name

      override def impl(dataIn: Seq[Any]): Seq[Any] = that.impl(old.impl(dataIn))

      override val metric = that.metric

      override var inputTypes = old.inputTypes
      override var outputTypes = that.outputTypes

      override var inputFormat = old.inputFormat
      override var outputFormat = that.outputFormat
      override var latency = old.latency + that.latency

      override def implH: ChainsawModule = new ChainsawModule(this) {
        val core0 = old.implH
        val core1 = that.implH
        core0.flowIn := flowIn
        core0 >> core1
        flowOut := core1.flowOut
      }
    }
  }

  // TODO: flowConverters can test itself
  //  def testItSelf() = ???

}
