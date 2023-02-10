package Chainsaw.dsp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._       // for finite state machine dialect
import spinal.lib.bus._       // for all kinds of bus and regIf
import spinal.lib.bus.regif._ // for regIf
import spinal.sim._           // for simulation
import spinal.core.sim._      // for more simulation

import Chainsaw._            // for basic templates
import Chainsaw.dsp._        // for dsp operators
import Chainsaw.arithmetic._ // for arithmetic operators
import Chainsaw.crypto._     // for crypto operators
import Chainsaw.xilinx._     // for Xilinx FPGA Flow

/** unwrap for normalized data(Pi -> 1)
  * @param numericType
  */
case class UnwrapPointByPoint(numericType: NumericType)
    extends ChainsawOperatorGenerator
    with FixedLatency {

  override def name: String = s"unwrap_${numericType}"

  override def inputTypes: Seq[NumericType] = Seq.fill(2)(numericType)

  override def outputTypes: Seq[NumericType] = Seq(numericType)

  override def impl(testCase: TestCase): Seq[BigDecimal] =
    unwrap(testCase.data).head.takeRight(1)

  override def metric(
      yours: Seq[BigDecimal],
      golden: Seq[BigDecimal]
  ): Boolean = true

  override def testCases: Seq[TestCase] = Seq.fill(3)(randomTestCase)
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(
    this
  ) {

    def scaleDown(data: AFix): AFix =
      (data * NumericType.SFix(0, 15).fromConstant(1 / scala.math.Pi))
        .fixTo(numericType())

    def scaleUp(data: AFix): AFix =
      (data * NumericType.SFix(2, 13).fromConstant(scala.math.Pi))
        .fixTo(numericType())

    val Seq(prev, next) = dataIn
    val fractional = numericType.fractional

    // stage 0
    val prevScaled = scaleDown(prev)
    val nextScaled = scaleDown(next)

    // stage 0
    val (m, l0)         = prevScaled.asBits.splitAt(fractional)
    val (n, l1)         = nextScaled.asBits.splitAt(fractional)
    val l1Main          = l1.takeHigh(fractional)

    val random = CounterFreeRun(13) // to avoid bias

    // 0 -> 1
    val mux0 = SInt(m.getBitsWidth bits)
    when(l1Main.asUInt > l0.asUInt)(mux0 := m.asSInt - 1)
      .elsewhen(l1Main.asUInt < l0.asUInt)(mux0 := m.asSInt + 1)
      .elsewhen(l1Main.asUInt === l0.asUInt && random.value.lsb)(
        mux0 := m.asSInt - 1
      )
      .otherwise(mux0 := m.asSInt + 1)
    // 1 -> 2
    val mux1 = Mux((m.lsb === n.lsb).d(1), m.asSInt.d(1), mux0).d(1)
    val ret  = numericType()
    ret.assignFromBits(mux1 ## l1.d(2))

    val unwrapped = numericType()
    unwrapped.assignFromBits(mux1 ## l1.d(2))

    dataOut.head := scaleUp(unwrapped)
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = ???

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil()

  override def fmaxEstimation: HertzNumber = 600 MHz

  override def resetCycle: Int = 1

  override def latency(): Int = 2
}
