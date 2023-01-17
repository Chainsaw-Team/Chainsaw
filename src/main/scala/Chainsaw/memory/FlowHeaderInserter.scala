package Chainsaw.memory

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/** insert a header before each sequence
  * @param header
  *   sequence of w-bit data, which will be inserted before each sequence
  * @param width
  *   width of the data(and the header)
  */
case class FlowHeaderInserter(
    header: Seq[BigInt],
    width: Int
) extends ChainsawInfiniteGenerator {

  override def implNaiveH = None

  override def name = s"FlowHeaderInserter_${hashName(header)}_${width}"

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation = VivadoUtil()

  override def fmaxEstimation = 600 MHz

  /** -------- interfaces
    * --------
    */
  override def inputTypes = Seq(NumericType.U(width))

  override def outputTypes = Seq(NumericType.U(width))

  override def impl(testCase: TestCase) =
    header.map(BigDecimal(_)) ++ testCase.data

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    ChainsawMetric.sameAsBigInt(yours, golden)

  override def testCases = {
    val lengths = Seq.fill(10)(scala.util.Random.nextInt(100) + 1)
    lengths.map(randomTestCase)
  }

  override def resetCycle = 1

  override def implH = new ChainsawInfiniteModule(this) {
    // buffering dataIn as upstream can't be stopped(is a flow)
    val dataInFifo = StreamFifo(flowIn.payload, 128)
    val overflow   = out Bool ()
    assert(!overflow)
    flowIn.toStream(overflow) >> dataInFifo.io.push

    val headerRom     = Mem(header.map(B(_, width bits)))
    val headerCounter = Counter(header.length)

    val fsm = new StateMachine {
      val WAITHEADER = makeInstantEntry() // for sync
      val HEADER     = new StateDelay(header.length)
      val DATA       = State()
      WAITHEADER.whenIsActive {
        when(dataInFifo.io.pop.valid)(goto(HEADER))
        dataInFifo.io.pop.ready := False
        validOut                := False
        dataOut.head.assignDontCare()
      }
      HEADER.whenIsActive {
        dataInFifo.io.pop.ready := False
        validOut                := True
        dataOut.head assignFromBits headerRom.readAsync(headerCounter.value)
        headerCounter.increment()
      }
      HEADER.whenCompleted {
        headerCounter.clear()
        goto(DATA)
      }
      DATA.whenIsActive {
        when(dataInFifo.io.pop.payload.last && dataInFifo.io.pop.valid) {
          when(dataInFifo.io.occupancy > 1)(goto(HEADER))
            .otherwise(goto(WAITHEADER))
        }
        dataInFifo.io.pop.ready := True
        validOut                := dataInFifo.io.pop.valid
        dataOut                 := dataInFifo.io.pop.payload.fragment
      }

      lastOut := dataInFifo.io.pop.payload.last && dataInFifo.io.pop.fire
    }
  }
}
