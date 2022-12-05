package Chainsaw

import spinal.core.sim._
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer

abstract class ChainsawBaseTest {

  val testName: String
  val gen: ChainsawBaseGenerator

  def simConfig = {
    val ret = SimConfig.workspaceName(testName).withFstWave
    ret._backend = gen.simBackEnd
    ret
  }


}

case class ChainsawOperatorTest(
                                 testName: String = "testTemp",
                                 gen: ChainsawOperatorGenerator,
                                 stimulus: Option[Seq[BigDecimal]] = None,
                                 golden: Option[Seq[BigDecimal]] = None
                               ) extends ChainsawBaseTest {

  import gen._

  /** --------
   * prepare
   * -------- */

  val dataInUse: Seq[BigDecimal] = stimulus.getOrElse(testCases)
  val inputVectorSize = if (inPortWidth == 0) 1 else inPortWidth // when inPortWidth is 0, the module is driven by clock
  val inputVectors = dataInUse.grouped(inputVectorSize).toSeq // vectors of raw data
  val goldenInUse = golden.getOrElse(inputVectors.flatMap(impl))
  val goldenVectors = goldenInUse.grouped(outPortWidth).toSeq // vectors of golden data

  require(inputVectors.last.length == inputVectorSize, s"data length ${dataInUse.length} is not a multiple of vector length $inPortWidth")
  require(goldenVectors.last.length == outPortWidth, s"golden length ${goldenInUse.length} is not a multiple of vector length $outPortWidth")
  require(inputVectors.length == goldenVectors.length, s"input vector count ${inputVectors.length} is not equal to golden vector count ${goldenVectors.length}")

  // data containers
  val outputVectors = ArrayBuffer[Seq[BigDecimal]]()
  val outputTimes = ArrayBuffer[Long]()

  /** --------
   * simulation
   * -------- */

  val simTimeMax = (26 // forkStimulus & flushing
    + inputVectors.length // poke
    + gen.latency // wait for last valid
    ) * 2

  simConfig.compile(gen.getImplH).doSim { dut =>
    import dut.{clockDomain, flowIn, flowOut}

    // init
    def init(): Unit = {
      flowIn.valid #= false
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
    }

    def poke(): SimThread = fork {
      var i = 0
      while (true) {
        if (i < inputVectors.length) {
          flowIn.payload.zip(inputVectors(i)).foreach { case (fix, decimal) => fix #= decimal }
          flowIn.valid #= true
        } else {
          // keep payload the same
          flowIn.valid #= false
        }
        i += 1
        clockDomain.waitSampling()
      }
    }

    def peek(): SimThread = fork {
      while (true) {
        if (flowOut.valid.toBoolean) {
          outputVectors += flowOut.payload.map(_.toBigDecimal)
          outputTimes += simTime()
        }
        clockDomain.waitSampling()
      }
    }

    def waitSimDone() = do clockDomain.waitSampling(10) while (simTime() <= simTimeMax)

    init()
    peek()
    poke()
    waitSimDone()
  }

  /** --------
   * check & show
   * -------- */
  require(outputVectors.length == goldenVectors.length, s"output vector count ${outputVectors.length} is not equal to golden vector count ${goldenVectors.length}")

  val remained = outputVectors.zip(goldenVectors).zipWithIndex // checking vector-by-vector
    .dropWhile { case ((y, g), _) => metric(y, g) }
  val success = remained.isEmpty
  val lastVector = remained.headOption.getOrElse(((outputVectors.last, goldenVectors.last), outputVectors.length - 1))
  val ((y, g), index) = lastVector
  val log = s"inputs: ${inputVectors(index).mkString(",")}\nyours : ${y.mkString(",")}\ngolden: ${g.mkString(",")}"

  if (!success) {
    logger.error(s"failed at $index-th cycle:\n$log")
  } else logger.info(s"test $testName passed\n$index-th cycle:\n$log")
  assert(success)
}

