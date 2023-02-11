package Chainsaw.dsp

import Chainsaw._
import Chainsaw.memory.DynamicTimeOut
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

// TODO: parallel version
case class DynamicDelay(delayMax: Int, dataType: NumericType, parallel: Int)
    extends ChainsawDynamicInfiniteGenerator
    with DynamicLatency {

  override def name = s"DynamicDelay_$delayMax"

  override def impl(testCase: TestCase) = testCase.data

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = {
    // test for 20 segments
    val controls = Seq
      .fill(20)((Random.nextInt(delayMax) + 4) min delayMax)
      .map(ctrl => Seq(BigDecimal(ctrl)))
    controls.map(ctrl =>
      TestCase(Seq.fill(100)(randomDataVector).flatten, ctrl)
    )
  }

  override def latency(control: Seq[BigDecimal]) = control.head.toInt

  override def controlTypes = Seq(NumericType.U(log2Up(delayMax + 1)))

  override def inputTypes = Seq.fill(parallel)(dataType)

  override def outputTypes = Seq.fill(parallel)(dataType)

  override def vivadoUtilEstimation = VivadoUtil()

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawDynamicInfiniteModule(this) {
    // TODO: for parallel > 1, using multiple following structures with different delays
    require(parallel == 1)
    // TODO: assertion for delay >= 4
    val control        = controlIn.head.asUInt()
    val currentLatency = RegNextWhen(control, validIn.rise())
    val counterWidth   = log2Up(delayMax + 1)
    val paddedLength   = pow2(counterWidth)
    val indexCounter   = CounterFreeRun(paddedLength)

    val flowWrite = cloneOf(flowOut)
    flowWrite.fragment := dataIn
    flowWrite.valid    := validIn
    flowWrite.last     := lastIn

    val ram = Mem(HardType(flowOut), paddedLength)
    ram.write(indexCounter.value, flowWrite)
    val readAddr = (indexCounter.value - (currentLatency - 3)).d()
    val readData = ram.readSync(readAddr).d()

    val timeOut = DynamicTimeOut(control)
    when(validIn.rise())(timeOut.clear())

    dataOut  := readData.fragment
    validOut := Mux(timeOut.state, readData.valid, False)
    lastOut  := Mux(timeOut, readData.last, False)
  }

  override def implNaiveH = None

  override def resetCycle = delayMax
}
