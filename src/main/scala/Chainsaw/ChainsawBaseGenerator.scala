package Chainsaw

import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim._

case class TestCase(
    data: Seq[BigDecimal],
    control: Seq[BigDecimal] = Seq[BigDecimal]()
) {
  override def toString =
    s"data   : ${data.take(100).mkString(",")}...\ncontrol: ${control.mkString(",")}"
}

object TestCase {
  def empty = TestCase(Seq[BigDecimal]())
}

trait ChainsawBaseGenerator {
  def name: String

  /** -------- performance
    * --------
    */
  def vivadoUtilEstimation: VivadoUtil

  def fmaxEstimation: HertzNumber

  /** -------- interfaces
    * --------
    */
  def inputTypes: Seq[NumericType]

  def outputTypes: Seq[NumericType]

  /** -------- model
    * --------
    */
  def impl(testCase: TestCase): Seq[BigDecimal]

  def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean

  def testCases: Seq[TestCase]

  def resetCycle: Int

  /** -------- implementations
    * --------
    */
  def simBackEnd: SpinalSimBackendSel = SpinalSimBackendSel.VERILATOR

  def implH: ChainsawBaseModule // core module, that is, the datapath

  // naive RTL implementation for simulation & top-down design, optional
  def implNaiveH: Option[ChainsawBaseModule]

  // TODO: is it useful?
  def implPass = new ChainsawBaseModule(this) {
    dataIn.foreach(_.addAttribute("dont_touch", "yes"))
    dataOut.foreach(_.assignDontCare())
    dataOut.foreach(_.addAttribute("dont_touch", "yes"))
  }

  // impl selection
  def setAsNaive(): Unit = naiveSet += className(this)

  def useNaive: Boolean = naiveSet.contains(className(this))

  def getImplH: ChainsawBaseModule = {

    def getNaive: ChainsawBaseModule = {
      logger.info(s"using naive implementation for $name")
      if (!atSimTime) implPass
      else
        implNaiveH match {
          case Some(impl) => impl
          case None =>
            throw new IllegalArgumentException("no naive implementation found")
        }
    }

    if (useNaive) getNaive
    else {
      val ret = implH
      if (ret == null) {
        logger.warn(s"use implNaiveH for $name as implH is null")
        getNaive
      } else ret
    }
  }

  def inPortWidth = inputTypes.length

  def outPortWidth = outputTypes.length

  /** -------- utils for input/output generation
    * --------
    */
  def emptyTypes: Seq[NumericType] = Seq[NumericType]()

  def clockInput(cycle: Int): Seq[BigDecimal] = Seq.fill(cycle)(BigDecimal(1))

  def randomDataVector: Seq[BigDecimal] =
    if (inputTypes.nonEmpty) inputTypes.map(_.random) else Seq(BigDecimal(0))

  def randomDataSequence(cycle: Int): Seq[BigDecimal] =
    Seq.fill(cycle)(randomDataVector).flatten

  def randomTestCase: TestCase

  /** -------- utils for test
    * --------
    */

  def doSelfTest() = ChainsawTest(s"test$name", this)

  def report: String =
    s"""
       |name: $name
       |input types: ${inputTypes.mkString(", ")}
       |output types: ${outputTypes.mkString(", ")}
       |utilEstimated: $vivadoUtilEstimation
       |fmaxEstimated: ${fmaxEstimation / 1e6} MHz
       |""".stripMargin

  /** -------- utils for instantiation
    * --------
    */
  def process(data: Seq[AFix]) = {
    val core = getImplH
    core.dataIn.zip(data).foreach { case (in, data) => in := data }
    core.dataOut
  }

  def process(data: Seq[AFix], validIn: Bool) = {
    val core = getImplH
    core.validIn := validIn
    core.dataIn.zip(data).foreach { case (in, data) => in := data }
    (core.dataOut, core.validOut)
  }

  def cloneInput = Vec(inputTypes.map(_.apply()))

  def cloneOutput = Vec(outputTypes.map(_.apply()))

  def ffioUtil = VivadoUtil(ff =
    inputTypes.map(_.bitWidth).sum + outputTypes.map(_.bitWidth).sum
  )
}

trait Dynamic {
  def controlTypes: Seq[NumericType]

  def controlPortWidth = controlTypes.length

  def randomControlVector = controlTypes.map(_.random)
}

trait Operator

trait Frame {

  def inputFrameFormat(control: Seq[BigDecimal]): FrameFormat

  def outputFrameFormat(control: Seq[BigDecimal]): FrameFormat

  def period(control: Seq[BigDecimal]) = {
    require(
      inputFrameFormat(control).period == outputFrameFormat(control).period,
      "input/output period should be the same"
    )
    inputFrameFormat(control).period
  }

  def randomInputFrame(control: Seq[BigDecimal]): Seq[BigDecimal]
}

trait SemiInfinite

trait FixedLatency {
  def latency(): Int
}

trait DynamicLatency {
  def latency(control: Seq[BigDecimal]): Int
}

trait OverwriteLatency

trait Unaligned {
  def inputTimes: Seq[Int]

  def inputInterval = inputTimes.max - inputTimes.min

  def outputTimes: Seq[Int]

  def outputInterval = outputTimes.max - outputTimes.min
}

trait Duty {
  def dutyRation: Double
}

trait ChainsawOperatorGenerator extends ChainsawBaseGenerator with Operator {

  override def resetCycle = 0

  override def implH: ChainsawOperatorModule

  override def implNaiveH: Option[ChainsawOperatorModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawOperatorModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawOperatorModule]

  override def randomTestCase = TestCase(randomDataVector)
}

trait ChainsawDynamicOperatorGenerator
    extends ChainsawBaseGenerator
    with Operator
    with Dynamic {

  override def resetCycle = 0

  override def implH: ChainsawDynamicOperatorModule

  override def implNaiveH: Option[ChainsawDynamicOperatorModule]

  override def implPass =
    super.implPass.asInstanceOf[ChainsawDynamicOperatorModule]

  override def getImplH =
    super.getImplH.asInstanceOf[ChainsawDynamicOperatorModule]

  override def randomTestCase = TestCase(randomDataVector, randomControlVector)
}

trait ChainsawFrameGenerator extends ChainsawBaseGenerator with Frame {

  def inputFrameFormat: FrameFormat

  def outputFrameFormat: FrameFormat

  override def inputFrameFormat(control: Seq[BigDecimal]) = inputFrameFormat

  override def outputFrameFormat(control: Seq[BigDecimal]) = outputFrameFormat

  override def implH: ChainsawFrameModule

  override def implNaiveH: Option[ChainsawFrameModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawFrameModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawFrameModule]

  def period: Int = {
    require(
      inputFrameFormat.period == outputFrameFormat.period,
      "input/output period should be the same"
    )
    inputFrameFormat.period
  }

  override def period(control: Seq[BigDecimal]) = period

  def randomInputFrame: Seq[BigDecimal] =
    Seq.fill(inputFrameFormat.period)(randomDataVector).flatten

  override def randomInputFrame(control: Seq[BigDecimal]) = randomInputFrame

  override def randomTestCase = TestCase(randomInputFrame)
}

trait ChainsawDynamicFrameGenerator
    extends ChainsawBaseGenerator
    with Frame
    with Dynamic {

  override def implH: ChainsawDynamicFrameModule

  override def implNaiveH: Option[ChainsawDynamicFrameModule]

  override def implPass =
    super.implPass.asInstanceOf[ChainsawDynamicFrameModule]

  override def getImplH =
    super.getImplH.asInstanceOf[ChainsawDynamicFrameModule]

  def randomInputFrame(control: Seq[BigDecimal]): Seq[BigDecimal] =
    Seq.fill(inputFrameFormat(control).period)(randomDataVector).flatten

  override def randomTestCase: TestCase = {
    val control = randomControlVector
    TestCase(randomInputFrame(control), control)
  }
}

trait ChainsawInfiniteGenerator
    extends ChainsawBaseGenerator
    with SemiInfinite {

  override def implH: ChainsawInfiniteModule

  override def implNaiveH: Option[ChainsawInfiniteModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawInfiniteModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawInfiniteModule]

  override def randomTestCase = TestCase(
    Seq.fill(resetCycle + 1)(randomDataVector).flatten
  )

  def randomTestCase(cycle: Int) = TestCase(
    Seq.fill(cycle)(randomDataVector).flatten
  )
}

trait ChainsawDynamicInfiniteGenerator
    extends ChainsawBaseGenerator
    with SemiInfinite
    with Dynamic {

  override def implH: ChainsawDynamicInfiniteModule

  override def implNaiveH: Option[ChainsawDynamicInfiniteModule]

  override def implPass =
    super.implPass.asInstanceOf[ChainsawDynamicInfiniteModule]

  override def getImplH =
    super.getImplH.asInstanceOf[ChainsawDynamicInfiniteModule]

  override def randomTestCase = TestCase(
    data    = Seq.fill(resetCycle + 1)(randomDataVector).flatten,
    control = randomControlVector
  )

  def randomTestCase(cycle: Int) = TestCase(
    data    = Seq.fill(cycle)(randomDataVector).flatten,
    control = randomControlVector
  )
}
