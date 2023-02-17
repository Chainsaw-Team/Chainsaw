package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim.SimThread

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import Chainsaw.io.pythonIo._

import java.io.File

case class ChainsawTest(
    testName: String = "testTemp",
    gen: ChainsawBaseGenerator,
    stimulus: Option[Seq[TestCase]]      = None,
    golden: Option[Seq[Seq[BigDecimal]]] = None,
    terminateAfter: Int                  = 10000,
    errorSegmentsShown: Int              = 10,
    doInterruptInsertion: Boolean        = true
) {

  import gen._

  def simConfig = {
    val spinalConfig = ChainsawSpinalConfig(gen)
    val ret = SimConfig
      .workspaceName(testName)
      .withFstWave
//      .allOptimisation
      .withConfig(spinalConfig)
    ret._backend = gen.simBackEnd
    ret
  }

  /** -------- get input segments
    * --------
    */
  val inputSegments: Seq[TestCase] = {
    val raw = stimulus.getOrElse(testCases)
    gen match {
      case frame: Frame =>
        raw.map { case TestCase(seg, control) =>
          val padded = frame.inputFrameFormat(control).fromRawToFrame(seg)
          TestCase(padded, control)
        }
      case _ => raw
    }
  }.sortBy(_.control.headOption.getOrElse(BigDecimal(0)))

  if (
    gen.isInstanceOf[SemiInfinite] && inputSegments.forall(
      _.data.length == inputSegments.head.data.length
    )
  )
    logger.warn(
      "all testCases have a same length, which may miss the problem in reset logic"
    )

  if (inPortWidth != 0) {
    val pass = gen match { // check data length
      case frame: Frame =>
        inputSegments.forall { case TestCase(data, control) =>
          data.length == frame.inputFrameFormat(control).allDataCount
        }
      case _: Operator =>
        inputSegments.map(_.data).forall(_.length == inPortWidth)
      case _ => true
    }
    require(pass, "input data length is not correct")
  }

  val targetSegmentCount = inputSegments.length
  val targetVectorCount =
    inputSegments.map(_.data.length).sum /
      (if (inPortWidth == 0) 1 else inPortWidth)

  /** -------- interrupts insertion
    * --------
    */
  val valids = inputSegments.map(testCase => (testCase, true))
  assert(
    valids.forall(_._1.data.nonEmpty),
    "empty segment, check your test cases generator"
  )

  val inputSegmentsWithInvalid = ArrayBuffer[(TestCase, Boolean)](valids.head)

  def insertInterrupts = {

    def getRandomControl: Seq[BigDecimal] = gen match {
      case dynamic: Dynamic => dynamic.randomControlVector
      case _                => Seq[BigDecimal]()
    }

    def getInterrupt(cycle: Int) = TestCase(
      Seq.fill(cycle)(randomDataVector).flatten,
      getRandomControl
    )

    def getRandomInterrupt = getInterrupt(Random.nextInt(10) + 1)

    def getResetInterrupt = getInterrupt(resetCycle max 1)

    val randomRatio = 10

    valids.tail.foreach { case (testCase, valid) =>
      val lastValidCycle =
        inputSegmentsWithInvalid.last._1.data.length /
          (if (inPortWidth == 0) 1 else inPortWidth)

      // insert reset interrupt before control change
      gen match { // for SemiInfinite, insert interrupt between segments
        case semiInfinite: SemiInfinite =>
          inputSegmentsWithInvalid += ((getResetInterrupt, false))
        case _ => // for other generators, insert interrupt when control change
          val controlChange =
            testCase.control != inputSegmentsWithInvalid.last._1.control
          if (controlChange)
            inputSegmentsWithInvalid += ((getResetInterrupt, false))
      }

      // insert random interrupt, probability = 1/randomRatio
      if (Random.nextInt(randomRatio) > randomRatio - 2)
        inputSegmentsWithInvalid += ((getRandomInterrupt, false))

      // insert null data for generators with trait Duty
      gen match {
        case duty: Duty =>
          if (duty.dutyRation < 1) {
            val nullCycle = (lastValidCycle * (1 - duty.dutyRation)).ceil.toInt
            inputSegmentsWithInvalid += ((getInterrupt(nullCycle), false))
          }
        case _ =>
      }

      inputSegmentsWithInvalid += ((testCase, valid))
    }
    inputSegmentsWithInvalid.insert(
      0,
      (getResetInterrupt, false)
    ) // for initialization
  }

  if (doInterruptInsertion) insertInterrupts else valids.foreach(inputSegmentsWithInvalid += _)

  val sortedValids = inputSegmentsWithInvalid.filter(_._2).map(_._1)

  /** -------- get golden segments
    * --------
    */
  val goldenSegments: Seq[Seq[BigDecimal]] = {
    val raw = golden.getOrElse(sortedValids.map(impl))
    gen match {
      case frame: Frame =>
        raw.zip(sortedValids).map { case (seg, TestCase(_, control)) =>
          frame.outputFrameFormat(control).fromRawToFrame(seg)
        }
      case _ => raw
    }
  }

  val latencies = gen match {
    case overwriteLatency: OverwriteLatency => sortedValids.map(_ => -1)
    case fixedLatency: FixedLatency =>
      sortedValids.map(_ => fixedLatency.latency())
    case dynamicLatency: DynamicLatency =>
      sortedValids.map(testCase => dynamicLatency.latency(testCase.control))
    case _ => sortedValids.map(_ => -1)
  }

  val pass = gen match { // check golden length
    case frame: Frame =>
      val validOutputFrameLengths = sortedValids.map(testCase => frame.outputFrameFormat(testCase.control).rawDataCount)
      goldenSegments.map(_.length).equals(validOutputFrameLengths)
    case _: Operator =>
      goldenSegments.forall(_.length == outPortWidth)
    case _ => true
  }
  require(pass, "golden data length is not correct")

  require(goldenSegments.length == targetSegmentCount)

  // segments -> vectors
  val inputVectorSize =
    if (inPortWidth == 0) 1
    else inPortWidth          // when inPortWidth is 0, the module is driven by clock
  val inputVectorsWithInValid // data, control, valid, last
      : mutable.Seq[(Seq[BigDecimal], Seq[BigDecimal], Boolean, Boolean)] =
    inputSegmentsWithInvalid.flatMap { case (TestCase(segment, control), valid) =>
      val dataVectors = segment.grouped(inputVectorSize).toSeq
      gen match { // for operator generator, last is always true
        case _: Operator =>
          dataVectors.map((_, control, valid, valid))
        case _ => // for frame and infinite generator, last is true at the last cycle of a sequence
          dataVectors.init.map(
            (_, control, valid, false)
          ) :+ (dataVectors.last, control, valid, valid)
      }
    }

  // data containers
  val outputVectors =
    ArrayBuffer[(Seq[BigDecimal], Boolean, Long)]() // (data,last,time)
  val inputSegmentTimes =
    ArrayBuffer[Long]() // starting cycle of each input segment

  /** -------- simulation
    * --------
    */

  logger.info(s"start simulation for $testName")
  logger.info(s"current naive list: ${naiveSet.mkString(" ")}")

  simConfig.compile(gen.getImplH).doSim { dut =>
    import dut.{clockDomain, flowInPointer, flowOutPointer}

    var currentSegments = 0
    var currentVectors  = 0
    var noValid         = 0

    // init
    def init(): Unit = {
      flowInPointer.valid #= false
      flowInPointer.last  #= false
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
    }

    var segmentsCounter = 1

    def poke(): SimThread = fork {
      var waitStart = true
      var i         = 0
      while (true) {
        if (i < inputVectorsWithInValid.length) {
          val (data, control, valid, last) = inputVectorsWithInValid(i)
          // data in
          flowInPointer.payload.zip(data).foreach { case (fix, decimal) =>
            fix #= decimal
          }
          // control in
          dut match {
            case dynamicModule: DynamicModule =>
              dynamicModule.controlIn.zip(control).foreach { case (fix, decimal) =>
                fix #= decimal
              }
            case _ => // no control
          }
          // valid & last
          flowInPointer.valid #= valid
          if (valid && waitStart) {
            inputSegmentTimes += simTime()
            waitStart = false
          }
          flowInPointer.last #= last
          if (last) {
            waitStart = true
            if (verbose >= 1) println(s"$segmentsCounter segment done")
            segmentsCounter += 1
          }
        } else {
          flowInPointer.valid #= false
          flowInPointer.last  #= false
        }
        i += 1
        clockDomain.waitSampling()
      }
    }

    val npzData: Map[ChainsawFlow, ArrayBuffer[BigDecimal]] =
      dut.monitoredFlows.map(flow => flow -> ArrayBuffer[BigDecimal]()).toMap

    def peek(): SimThread = fork {
      while (true) {
        if (flowOutPointer.valid.toBoolean) {
          val tuple = (
            flowOutPointer.payload.map(_.toBigDecimal),
            flowOutPointer.last.toBoolean,
            simTime()
          )
          outputVectors += tuple
          if (flowOutPointer.last.toBoolean) currentSegments += 1
        }

        dut.monitoredFlows.foreach { flow =>
          if (flow.valid.toBoolean) {
            npzData(flow) ++= flow.fragment.map(_.toBigDecimal)
          }
        }

        clockDomain.waitSampling()
      }
    }

    // working in the main thread, pushing the simulation forward
    def waitSimDone(): Unit =
      while (currentSegments < targetSegmentCount && noValid < terminateAfter) {
        if (outputVectors.length == currentVectors) noValid += 100
        currentVectors = outputVectors.length
        clockDomain.waitSampling(100)
      }

    init()
    peek()
    poke()
    waitSimDone()

    logger.info(s"simulation done for $testName")

    if (noValid >= terminateAfter)
      logger.error(
        s"Simulation terminated after $terminateAfter cycles of no valid output"
      )

    npzData.foreach { case (flow, decimals) => exportSignal(new File(s"${flow.getName()}.npz"), decimals) }
    logger.info("data exported")
  }

  /** -------- check & show
    * --------
    */
  // vectors -> segments

  // output data organized by segments
  var outputSegments     = Seq[Seq[BigDecimal]]()
  var outputSegmentTimes = Seq[Long]() // starting cycle of each output segment

  val outputTimes = outputVectors.map(_._3)
  gen match { // outputVector -> outputSegments
    case _: Operator => // for operator, each vector is a segment
      outputSegments     = outputVectors.map(_._1)
      outputSegmentTimes = outputTimes
    // for frame and infinite, get segments according to last
    case _ =>
      val lasts = outputVectors.map(_._2)
      val lastPoints = lasts.zipWithIndex
        .filter(_._1 == true)
        .map(_._2) // indices where last is true
      val starts      = lastPoints.map(_ + 1)
      val splitPoints = 0 +: starts
      outputSegments = splitPoints
        .sliding(2)
        .map { case Seq(start, end) =>
          if (verbose >= 1)
            logger.info(
              s"output segment: ${outputTimes(start)} -> ${outputTimes(end)}"
            )
          outputVectors.slice(start, end)
        }
        .map(_.flatMap(_._1))
        .toSeq

      outputSegmentTimes = splitPoints.init.map(outputTimes)
  }

  require(
    outputSegments.length == goldenSegments.length,
    s"output segment count ${outputSegments.length} is not equal to golden segment count ${goldenSegments.length}"
  )

  val actualLatencies = outputSegmentTimes.zip(inputSegmentTimes).map { case (outputTime, inputTime) =>
    (outputTime - 2 - inputTime) / 2
  }

  val passRecord = inputSegments.indices.map { i =>
    val payloadPass = metric(outputSegments(i), goldenSegments(i))
    val latencyPass = actualLatencies(i) == latencies(i) || latencies(i) == -1
    payloadPass && latencyPass
  }

  val success = passRecord.forall(_ == true)

  def log(index: Int) = {
    s"-----$index-th segment-----\n" +
      s"${sortedValids(index)}\n" +
      // TODO: show ...
      s"yours  : ${outputSegments(index).take(100).mkString(",")}\n" +
      s"golden : ${goldenSegments(index).take(100).mkString(",")}\n" +
      s"expected latency: ${if (latencies(index) < 0) "undetermined"
      else latencies(index).toString}, " +
      s"actual latency: ${actualLatencies(index)}\n"
  }

  val logIndices =
    if (success) Seq(outputSegments.length - 1)
    else passRecord.zipWithIndex.filter(!_._1).map(_._2)
  val allLog = logIndices.take(errorSegmentsShown).map(log).mkString("\n")

  if (!success) logger.error(s"failures:\n$allLog")
  else logger.info(s"test $testName passed\n$allLog")
  assert(success)
}

object ChainsawTestWithData {
  def apply(
      testName: String,
      gen: ChainsawBaseGenerator,
      data: Seq[TestCase],
      golden: Seq[Seq[BigDecimal]]
  ) =
    ChainsawTest(testName, gen, Some(data), Some(golden))
}
