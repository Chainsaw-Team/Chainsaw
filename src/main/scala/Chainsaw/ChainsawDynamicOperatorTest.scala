package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class ChainsawDynamicOperatorTest(
                                        testName: String = "testTemp",
                                        gen: ChainsawDynamicOperatorGenerator,
                                        stimulus: Option[Seq[TestCase]] = None,
                                        golden: Option[Seq[Seq[BigDecimal]]] = None,
                                        terminateAfter: Int = 10000,
                                        errorSegmentsShown: Int = 10
                                      ) extends ChainsawBaseTest {

  import gen._

  /** --------
   * prepare
   * -------- */
  // get input segments
  val dataInUse: Seq[TestCase] = stimulus.getOrElse(testCases)
  require(dataInUse.forall { case TestCase(data, control) => (data.length == inPortWidth || inPortWidth == 0) && control.length == controlPortWidth })
  val targetSegmentCount = dataInUse.length
  // segments -> vectors
  val inputVectorSize = if (inPortWidth == 0) 1 else inPortWidth // when inPortWidth is 0, the module is driven by clock

  // invalid data insertion between segments
  val valids = dataInUse.map(testCase => (testCase, true))
  val invalids = Seq.fill(targetSegmentCount / 3)((randomTestcase, false))
  val inputSegmentsWithInvalid = Random.shuffle(valids ++ invalids)
  val sortedValids = inputSegmentsWithInvalid.filter(_._2).map(_._1)

  // get golden segments
  val goldenInUse: Seq[Seq[BigDecimal]] = golden.getOrElse(sortedValids.map(impl))
  val latencies = sortedValids.map(testCase => latency(testCase.control))
  require(goldenInUse.forall(_.length == outPortWidth))
  require(goldenInUse.length == targetSegmentCount)

  // data containers
  val outputVectors = ArrayBuffer[Seq[BigDecimal]]()
  val inputTimes = ArrayBuffer[Long]()
  val outputTimes = ArrayBuffer[Long]()

  /** --------
   * simulation
   * -------- */

  simConfig.compile(gen.getImplH).doSim { dut =>
    import dut.{clockDomain, controlIn, flowIn, flowOut}

    // init
    def init(): Unit = {
      flowIn.valid #= false
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
    }

    def poke(): SimThread = fork {
      var i = 0
      while (true) {
        if (i < inputSegmentsWithInvalid.length) {
          val (testCase, valid) = inputSegmentsWithInvalid(i)
          flowIn.payload.zip(testCase.data).foreach { case (fix, decimal) => fix #= decimal }
          controlIn.zip(testCase.control).foreach { case (fix, decimal) => fix #= decimal }
          flowIn.valid #= valid
          if(valid) inputTimes += simTime()
        } else {
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


    var currentSegments = 0
    var noValid = 0

    // working in the main thread, pushing the simulation forward
    def waitSimDone(): Unit =
      while (currentSegments < targetSegmentCount && noValid < terminateAfter) {
        if (outputVectors.length == currentSegments) noValid += 100
        currentSegments = outputVectors.length
        clockDomain.waitSampling(100)
      }

    init()
    peek()
    poke()
    waitSimDone()

    if (noValid >= terminateAfter) logger.error(s"Simulation terminated after $terminateAfter cycles of no valid output")
  }

  /** --------
   * check & show
   * -------- */
  require(outputVectors.length == goldenInUse.length,
    s"output vector count ${outputVectors.length} is not equal to golden vector count ${goldenInUse.length}")
  val actualLatencies = outputTimes.zip(inputTimes).map { case (outputTime, inputTime) => (outputTime - 2 - inputTime) / 2 }

  val passRecord = dataInUse.indices.map { i =>
    val payloadPass = metric(outputVectors(i), goldenInUse(i))
    val latencyPass = actualLatencies(i) == latencies(i)
    payloadPass && latencyPass
  }

  val success = passRecord.forall(_ == true)

  def log(index: Int) = {
    s"-----$index-th segment-----\n" +
      s"${sortedValids(index)}\n" +
      s"yours  : ${outputVectors(index).mkString(",")}\n" +
      s"golden : ${goldenInUse(index).mkString(",")}\n" +
      s"expected latency: ${latencies(index)}, actual latency: ${actualLatencies(index)}\n"
  }

  val logIndices = if (success) Seq(outputVectors.length - 1) else passRecord.zipWithIndex.filter(!_._1).map(_._2)
  val allLog = logIndices.take(errorSegmentsShown).map(log).mkString("\n")

  if (!success) logger.error(s"failures:\n$allLog")
  else logger.info(s"test $testName passed\n$allLog")
  assert(success)
}