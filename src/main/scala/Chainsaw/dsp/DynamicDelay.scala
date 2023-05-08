package Chainsaw.dsp

import Chainsaw._
import Chainsaw.memory.DynamicTimeOut
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

/** input = (x, c), output = xz^-c^
 */
case class DynamicDelay(delayMax: Int, dataType: NumericType)
  extends ChainsawDynamicInfiniteGenerator
    with DynamicLatency {

  override def name = s"DynamicDelay_$delayMax"

  override def impl(testCase: TestCase) = testCase.data

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = {
    val controls =
      Seq.fill(20)((Random.nextInt(delayMax) + 4) min delayMax).map(ctrl => Seq(BigDecimal(ctrl)))
    controls.map(ctrl => TestCase(Seq.fill(100)(randomDataVector).flatten, ctrl))
  }

  override def latency(control: Seq[BigDecimal]) = control.head.toInt

  override def controlTypes = Seq(NumericType.U(log2Up(delayMax + 1)))

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(dataType)

  override def vivadoUtilEstimation = VivadoUtil()

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawDynamicInfiniteModule(this) {

    val delay = controlIn.head.asUInt()
//    assert(delay >= U(4))

    val currentDelay = RegNextWhen(delay, validIn.rise()) // lock the control value

    val counterWidth = log2Up(delayMax + 1)
    val paddedLength = pow2(counterWidth)
    val indexCounter = CounterFreeRun(paddedLength)

    val ram = Mem(HardType(flowIn), paddedLength)
    ram.write(indexCounter.value, flowIn)

    // +2 for read latency
    // +1 for address latency(calculation)
    // -currentDelay for delay
    val readAddr = (indexCounter.value - (currentDelay - 3)).d()
    val readData = ram.readSync(readAddr).d()

    if (globalData.toplevel == this || dspStrict) {
      val timeOut = DynamicTimeOut(delay) // protection after delay changed
      when(validIn.rise())(timeOut.clear())
      dataOut  := readData.fragment
      validOut := Mux(timeOut, readData.valid, False)
      lastOut  := Mux(timeOut, readData.last, False)
    } else readData >> flowOut
  }

  override def implNaiveH = None

  override def resetCycle = delayMax
}
