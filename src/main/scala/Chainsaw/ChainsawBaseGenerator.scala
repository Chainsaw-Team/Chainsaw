package Chainsaw

import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim._

case class TestCase(data: Seq[BigDecimal], control: Seq[BigDecimal]) {
  override def toString = s"data   : ${data.mkString(",")}\ncontrol: ${control.mkString(",")}"
}

trait ChainsawBaseGenerator {
  def name: String

  def vivadoUtilEstimation: VivadoUtil

  def fmaxEstimation: HertzNumber

  def inputTypes: Seq[NumericTypeNew]

  def outputTypes: Seq[NumericTypeNew]

  /** --------
   * implementations
   * -------- */
  def simBackEnd: SpinalSimBackendSel = SpinalSimBackendSel.VERILATOR

  def implH: ChainsawBaseModule // core module, that is, the datapath

  def implNaiveH: Option[ChainsawBaseModule] // naive RTL implementation for simulation & top-down design, optional

  // TODO: better implementation
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

  def randomInputVector = inputTypes.map(_.random)
}

trait ChainsawOperatorGenerator extends ChainsawBaseGenerator {
  def impl(data: Seq[BigDecimal]): Seq[BigDecimal]

  def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean

  def testCases: Seq[BigDecimal]

  def latency: Int

  override def implH: ChainsawOperatorModule

  override def implNaiveH: Option[ChainsawOperatorModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawOperatorModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawOperatorModule]
}

trait ChainsawDynamicOperatorGenerator extends ChainsawBaseGenerator {

  def impl(testCase: TestCase): Seq[BigDecimal]

  def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean

  /** Sequence of data & control
   */
  def testCases: Seq[TestCase]

  def latency(control: Seq[BigDecimal]): Int

  def controlTypes: Seq[NumericTypeNew]

  def controlPortWidth = controlTypes.length

  override def implH: ChainsawDynamicOperatorModule

  override def implNaiveH: Option[ChainsawDynamicOperatorModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawDynamicOperatorModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawDynamicOperatorModule]

  def randomTestcase = TestCase(inputTypes.map(_.random), controlTypes.map(_.random))
}

trait ChainsawFrameGenerator extends ChainsawBaseGenerator {
  def impl(data: Seq[BigDecimal]): Seq[BigDecimal]

  def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean

  def testCases: Seq[Seq[BigDecimal]]

  def latency: Int

  def inputFrameFormat: FrameFormat

  def outputFrameFormat: FrameFormat

  override def implH: ChainsawFrameModule

  override def implNaiveH: Option[ChainsawFrameModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawFrameModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawFrameModule]

  def period = {
    require(inputFrameFormat.period == outputFrameFormat.period, "input/output period should be the same")
    inputFrameFormat.period
  }

  def randomInputFrame = Seq.fill(inputFrameFormat.period)(randomInputVector).flatten
}

trait ChainsawDynamicFrameGenerator extends ChainsawBaseGenerator {
  def impl(data: TestCase): Seq[BigDecimal]

  def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean

  def testCases: Seq[TestCase]

  def latency(control: Seq[BigDecimal]): Int

  def resetCycle: Int

  def controlTypes: Seq[NumericTypeNew]

  def controlPortWidth = controlTypes.length

  def inputFrameFormat(control: Seq[BigDecimal]): FrameFormat

  def outputFrameFormat(control: Seq[BigDecimal]): FrameFormat

  override def implH: ChainsawDynamicFrameModule

  override def implNaiveH: Option[ChainsawDynamicFrameModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawDynamicFrameModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawDynamicFrameModule]

  def period(control: Seq[BigDecimal]) = {
    require(inputFrameFormat(control).period == outputFrameFormat(control).period, "input/output period should be the same")
    inputFrameFormat(control)
  }

  def randomInputFrame(control: Seq[BigDecimal]) =
    Seq.fill(inputFrameFormat(control).period)(randomInputVector).flatten

  def randomControl = controlTypes.map(_.random)

  def randomTestCase = {
    val control = randomControl
    TestCase(randomInputFrame(control), control)
  }
}

trait ChainsawInfiniteGenerator extends ChainsawBaseGenerator {

  def impl(data: Seq[BigDecimal]): Seq[BigDecimal]

  def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean

  def testCases: Seq[Seq[BigDecimal]]

  def latency: Int

  def resetCycle: Int

  override def implH: ChainsawInfiniteModule

  override def implNaiveH: Option[ChainsawInfiniteModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawInfiniteModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawInfiniteModule]

}

trait ChainsawDynamicInfiniteGenerator extends ChainsawBaseGenerator {

  def impl(testCase: TestCase): Seq[BigDecimal]

  def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean

  def testCases: Seq[TestCase]

  def controlTypes: Seq[NumericTypeNew]

  def controlPortWidth = controlTypes.length

  def latency(control: Seq[BigDecimal]): Int

  def resetCycle: Int

  override def implH: ChainsawDynamicInfiniteModule

  override def implNaiveH: Option[ChainsawDynamicInfiniteModule]

  override def implPass = super.implPass.asInstanceOf[ChainsawDynamicInfiniteModule]

  override def getImplH = super.getImplH.asInstanceOf[ChainsawDynamicInfiniteModule]

  def randomControl = controlTypes.map(_.random)

}