package Chainsaw

import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim._

case class TestCase(data: Seq[BigDecimal], control: Seq[BigDecimal] = Seq[BigDecimal]()) {
  override def toString =
    s"data   : ${data.mkString(",")}\ncontrol: ${control.mkString(",")}"
}

trait ChainsawBaseGenerator {
  def name: String

  def vivadoUtilEstimation: VivadoUtil

  def fmaxEstimation: HertzNumber

  def inputTypes: Seq[NumericTypeNew]

  def outputTypes: Seq[NumericTypeNew]

  /** --------
   * model
   * -------- */
  def impl(testCase: TestCase): Seq[BigDecimal]

  def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean

  def testCases: Seq[TestCase]

  def latency(control: Seq[BigDecimal]): Int

  def resetCycle: Int

  /** --------
   * implementations
   * -------- */
  def simBackEnd: SpinalSimBackendSel = SpinalSimBackendSel.VERILATOR

  def implH: ChainsawBaseModule // core module, that is, the datapath

  def implNaiveH: Option[ChainsawBaseModule] // naive RTL implementation for simulation & top-down design, optional

  def implPass = new ChainsawBaseModule(this) {
    dataIn.foreach(_.addAttribute("dont_touch", "yes"))
    dataOut.foreach(_.assignDontCare())
    dataOut.foreach(_.addAttribute("dont_touch", "yes"))
  }

  // impl selection
  def setAsNaive(): Unit = naiveSet += this.getClass.getSimpleName

  def useNaive: Boolean = naiveSet.contains(this.getClass.getSimpleName)

  def getImplH: ChainsawBaseModule = {
    if (useNaive) {
      if (!atSimTime) implPass
      else implNaiveH match {
        case Some(impl) => impl
        case None => throw new IllegalArgumentException("no naive implementation found")
      }
    }
    else implH
  }

  def inPortWidth = inputTypes.length

  def outPortWidth = outputTypes.length

  /** --------
   * utils for input/output generation
   * -------- */

  def emptyTypes: Seq[NumericTypeNew] = Seq[NumericTypeNew]()

  def clockInput(cycle: Int) = Seq.fill(cycle)(BigDecimal(1))

  def randomDataVector = inputTypes.map(_.random)

  def randomTestCase: TestCase
}

trait SemiInfinite

trait Operator

trait Dynamic {
  def controlTypes: Seq[NumericTypeNew]

  def controlPortWidth = controlTypes.length

  def randomControlVector = controlTypes.map(_.random)
}

trait FixedLatency {
  def latency(): Int
}

trait Frame {

  def inputFrameFormat(control: Seq[BigDecimal]): FrameFormat

  def outputFrameFormat(control: Seq[BigDecimal]): FrameFormat

  def period(control: Seq[BigDecimal]) = {
    require(inputFrameFormat(control).period == outputFrameFormat(control).period, "input/output period should be the same")
    inputFrameFormat(control).period
  }

  def randomInputFrame(control: Seq[BigDecimal]): Seq[BigDecimal]
}

trait ChainsawOperatorGenerator extends ChainsawBaseGenerator with Operator with FixedLatency {

  override def resetCycle = 0

  override def latency(control: Seq[BigDecimal]) = latency()

  override def implH: ChainsawOperatorModule

  override def implNaiveH: Option[ChainsawOperatorModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawOperatorModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawOperatorModule]

  override def randomTestCase = TestCase(randomDataVector)
}

trait ChainsawDynamicOperatorGenerator extends ChainsawBaseGenerator with Operator with Dynamic {

  override def resetCycle = 0

  override def implH: ChainsawDynamicOperatorModule

  override def implNaiveH: Option[ChainsawDynamicOperatorModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawDynamicOperatorModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawDynamicOperatorModule]

  override def randomTestCase = TestCase(randomDataVector, randomControlVector)
}

trait ChainsawFrameGenerator extends ChainsawBaseGenerator with FixedLatency with Frame {

  override def latency(control: Seq[BigDecimal]) = latency()

  def inputFrameFormat: FrameFormat

  def outputFrameFormat: FrameFormat

  override def inputFrameFormat(control: Seq[BigDecimal]) = inputFrameFormat

  override def outputFrameFormat(control: Seq[BigDecimal]) = inputFrameFormat

  override def implH: ChainsawFrameModule

  override def implNaiveH: Option[ChainsawFrameModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawFrameModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawFrameModule]

  def period: Int = {
    require(inputFrameFormat.period == outputFrameFormat.period, "input/output period should be the same")
    inputFrameFormat.period
  }

  override def period(control: Seq[BigDecimal]) = period

  def randomInputFrame: Seq[BigDecimal] = Seq.fill(inputFrameFormat.period)(randomDataVector).flatten

  override def randomInputFrame(control: Seq[BigDecimal]) = randomInputFrame

  override def randomTestCase = TestCase(randomInputFrame)
}

trait ChainsawDynamicFrameGenerator extends ChainsawBaseGenerator with Frame with Dynamic {

  override def implH: ChainsawDynamicFrameModule

  override def implNaiveH: Option[ChainsawDynamicFrameModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawDynamicFrameModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawDynamicFrameModule]

  def randomInputFrame(control: Seq[BigDecimal]): Seq[BigDecimal] =
    Seq.fill(inputFrameFormat(control).period)(randomDataVector).flatten

  override def randomTestCase: TestCase = {
    val control = randomControlVector
    TestCase(randomInputFrame(control), control)
  }
}

trait ChainsawInfiniteGenerator extends ChainsawBaseGenerator with FixedLatency with SemiInfinite {

  override def latency(control: Seq[BigDecimal]) = latency()

  override def implH: ChainsawInfiniteModule

  override def implNaiveH: Option[ChainsawInfiniteModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawInfiniteModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawInfiniteModule]

  override def randomTestCase = TestCase(Seq.fill(resetCycle)(randomDataVector).flatten)
}

trait ChainsawDynamicInfiniteGenerator extends ChainsawBaseGenerator with Dynamic with SemiInfinite {

  override def implH: ChainsawDynamicInfiniteModule

  override def implNaiveH: Option[ChainsawDynamicInfiniteModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawDynamicInfiniteModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawDynamicInfiniteModule]

  override def randomTestCase = TestCase(Seq.fill(resetCycle)(randomDataVector).flatten, randomControlVector)
}