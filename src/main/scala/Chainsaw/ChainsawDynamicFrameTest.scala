package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class ChainsawDynamicFrameTest(
                                     testName: String = "testTemp",
                                     gen: ChainsawDynamicFrameGenerator,
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
  val inputSegments: Seq[TestCase] = stimulus.getOrElse(testCases).sortBy(_.control.head)
  require(inputSegments.forall { case TestCase(data, control) =>
    (data.length == inputFrameFormat(control).rawDataCount || inPortWidth == 0) &&
      control.length == controlPortWidth
  })

  val targetSegmentCount = inputSegments.length
  val targetVectorCount = inputSegments.map(_.data.length).sum / inPortWidth
  logger.info("target vector count: " + targetVectorCount)
  // invalid data insertion between segments
  val valids = inputSegments.map(testCase => (testCase, true))

  def getSwitchInterrupt = TestCase(Seq.fill(resetCycle)(randomInputVector).flatten, randomControl)
  def getRandomInterrupt = TestCase(Seq.fill(Random.nextInt(10))(randomInputVector).flatten, randomControl)

  val randomRatio = 10
  val inputSegmentsWithInvalid = ArrayBuffer[(TestCase, Boolean)](valids.head)
  valids.tail.foreach { case (testCase, valid) =>
    if (!testCase.control.equals(inputSegmentsWithInvalid.last._1.control)) {
      logger.info("switch interrupt inserted")
      inputSegmentsWithInvalid += ((getSwitchInterrupt, false))
    }
    if (Random.nextInt(randomRatio) > randomRatio - 2) {
      inputSegmentsWithInvalid += ((getRandomInterrupt, false))
      logger.info("random interrupt inserted")
    } // probability = 1/ratio
    inputSegmentsWithInvalid += ((testCase, valid))
  }


  val sortedValids = inputSegmentsWithInvalid.filter(_._2).map(_._1)

  // get golden segments
  val goldenSegments: Seq[Seq[BigDecimal]] = golden.getOrElse(sortedValids.map(impl))
  val latencies = sortedValids.map(testCase => latency(testCase.control))
  val validOutputFrameLengths = sortedValids.map(testCase => outputFrameFormat(testCase.control).rawDataCount)
  require(goldenSegments.map(_.length).equals(validOutputFrameLengths))
  require(goldenSegments.length == targetSegmentCount)

  // segments -> vectors
  val inputVectorSize = if (inPortWidth == 0) 1 else inPortWidth // when inPortWidth is 0, the module is driven by clock
  val inputVectorsWithValid = inputSegmentsWithInvalid.flatMap { case (TestCase(segment, control), valid) =>
    segment.grouped(inPortWidth).toSeq.map((_, control, valid))
  }

  // data containers
  val outputVectors = ArrayBuffer[Seq[BigDecimal]]()
  val inputTimes = ArrayBuffer[Long]()
  val outputTimes = ArrayBuffer[Long]()

  /** --------
   * simulation
   * -------- */

  simConfig.compile(gen.getImplH).doSim { dut =>
    import dut.{clockDomain, flowIn, flowOut, controlIn}

    // init
    def init(): Unit = {
      flowIn.valid #= false
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
    }

    def poke(): SimThread = fork {
      var i = 0
      while (true) {
        if (i < inputVectorsWithValid.length) {
          val (data, control, valid) = inputVectorsWithValid(i)
          flowIn.payload.zip(data).foreach { case (fix, decimal) => fix #= decimal }
          controlIn.zip(control).foreach { case (fix, decimal) => fix #= decimal }
          flowIn.valid #= valid
          if (valid) inputTimes += simTime()
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

    var currentVectors = 0
    var noValid = 0

    // working in the main thread, pushing the simulation forward
    def waitSimDone(): Unit =
      while (currentVectors < targetVectorCount && noValid < terminateAfter) {
        if (outputVectors.length == currentVectors) noValid += 100
        currentVectors = outputVectors.length
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
  // vectors -> segments
  val outputCycleCounts = sortedValids.map(testCase => outputFrameFormat(testCase.control).period)
  val slices = outputCycleCounts.scan(0)(_ + _).prevAndNext { case (start, end) => start until end }
  val outputSegments = slices.map(slice => outputVectors.slice(slice.start, slice.end)).map(_.flatten)
  val inputSegmentTimes = slices.map(slice => inputTimes.slice(slice.start, slice.end)).map(_.head)
  val outputSegmentTimes = slices.map(slice => outputTimes.slice(slice.start, slice.end)).map(_.head)

  require(outputSegments.length == goldenSegments.length,
    s"output segment count ${outputSegments.length} is not equal to golden segment count ${goldenSegments.length}")

  val actualLatencies = outputTimes.zip(inputTimes).map { case (outputTime, inputTime) => (outputTime - 2 - inputTime) / 2 }

  val passRecord = inputSegments.indices.map { i =>
    val payloadPass = metric(outputSegments(i), goldenSegments(i))
    val latencyPass = actualLatencies(i) == latencies(i)
    payloadPass && latencyPass
  }

  val success = passRecord.forall(_ == true)

  def log(index: Int) = {
    s"-----$index-th segment-----\n" +
      s"${sortedValids(index)}\n" +
      s"yours  : ${outputSegments(index).mkString(",")}\n" +
      s"golden : ${goldenSegments(index).mkString(",")}\n" +
      s"expected latency: ${latencies(index)}, actual latency: ${actualLatencies(index)}\n"
  }

  val logIndices = if (success) Seq(outputSegments.length - 1) else passRecord.zipWithIndex.filter(!_._1).map(_._2)
  val allLog = logIndices.take(errorSegmentsShown).map(log).mkString("\n")

  if (!success) logger.error(s"failures:\n$allLog")
  else logger.info(s"test $testName passed\n$allLog")
  assert(success)
}