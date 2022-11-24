package Chainsaw

import spinal.core._

import scala.language.postfixOps
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

sealed trait ImplMode

object Comb extends ImplMode

object StateMachine extends ImplMode

object Infinite extends ImplMode

import Chainsaw.xilinx._

trait ChainsawGenerator {

  def name = getAutoName(this)

  def getAutoName[T: TypeTag](obj: T) = {
    val fieldSymbols = typeOf[T].members.filter(_.isMethod).map(_.asTerm).filter(_.isCaseAccessor).toSeq.reverse
    val fieldNames = fieldSymbols.map(_.name)
    val instanceMirror: universe.InstanceMirror = mirror.reflect(this)
    val valuesAndNames = fieldNames.map(_.toString).zip(fieldSymbols.map(instanceMirror.reflectField).map(_.get)).sortBy(_._1)

    def getFieldName(name: String, value: Any) = {
      value match {
        case boolean: Boolean => if (boolean) name.trim else s"not${name.trim}"
        case bigInt: BigInt => hashName(bigInt)
        case seq: Seq[_] => hashName(seq)
        case solution: ChainsawSolution => hashName(solution)
        case operatorType: OperatorType => className(operatorType)
        case chainsawEnum: ChainsawEnum => className(chainsawEnum)
        case _ => value.toString
      }
    }

    val fieldName = valuesAndNames.map { case (name, value) =>
      value match {
        case option: Option[_] => option match {
          case Some(some) => getFieldName(name, some)
          case None => "none"
        }
        case _ => getFieldName(name, value)
      }
    }.mkString("_")
    className(this) + "_" + fieldName
  }

  /** -------- golden model --------
   *
   */
  def impl(dataIn: Seq[Any]): Seq[Any] // golden model

  val implMode: ImplMode = Comb

  val metric: ChainsawMetric = ChainsawMetric.defaultMetric

  def generateTestCases: Seq[Any] = null

  /** -------- size information --------
   *
   */
  def inputTypes: Seq[NumericType]
  def outputTypes: Seq[NumericType]

  /** -------- timing information --------
   *
   */
  def inputFormat: FrameFormat
  def outputFormat: FrameFormat
  val inputTimes: Option[Seq[Int]] = None // when this is empty, inputs are aligned
  val outputTimes: Option[Seq[Int]] = None
  def latency: Int // defined as the latency from the head of inputs to the head of outputs
  def offset: Int = 0

  /** -------- performance information --------
   *
   */
  def utilEstimation: VivadoUtil = VivadoUtilRequirement()
  def fmaxEstimation: HertzNumber = 600 MHz

  /** -------- implementations --------
   *
   */
  def implH: ChainsawModule // core module, that is, the datapath

  def implNaiveH: Option[ChainsawModule] = None // naive RTL implementation for simulation & top-down design, optional

  def implPass: ChainsawModule = new ChainsawModule(this) {
    dataIn.foreach(_.addAttribute("dont_touch", "yes"))
    dataOut.foreach(_.assignDontCare())
    dataOut.foreach(_.addAttribute("dont_touch", "yes"))
  }

  def doSelfTest(): ChainsawTestReport = ChainsawTest(s"test_$name", this, generateTestCases).doTest()

  def setAsNaive(): Unit = naiveSet += this.getClass.getSimpleName

  def useNaive: Boolean = naiveSet.contains(this.getClass.getSimpleName)

  def getImplH: ChainsawModule = {
    doDrc()
    if (useNaive && !atSimTime) implPass
    else if (useNaive) implNaiveH.getOrElse(implH)
    else implH
  }

  /** -------- utils --------
   *
   */
  // when a module need no control, you can use it as a function
  def asFunc: Seq[Bits] => Seq[Bits] = (dataIn: Seq[Bits]) => {
    require(needNoControl)
    val core = implH
    core.setFreeRun()
    core.dataIn := Vec(dataIn)
    core.dataOut
  }

  def doDrc(): Unit = {
    assert(inputFormat.period == outputFormat.period, s"input: ${inputFormat.period}, output: ${outputFormat.period}")
    assert(inputFormat.portSize == inputTypes.length, s"inputFrame port number: ${inputFormat.portSize}, actual input port number: ${inputTypes.length}")
    assert(outputFormat.portSize == outputTypes.length, s"outputFrame port number: ${outputFormat.portSize}, actual output port number: ${outputTypes.length}")

    assert(actualInTimes.head == 0, s"${actualInTimes.mkString(" ")}")
    assert(actualInTimes.length == inputWidths.length)
    assert(actualInTimes.forall(_ >= 0))
    assert(actualOutTimes.head == 0)
    assert(actualOutTimes.length == outputWidths.length)
    assert(actualOutTimes.forall(_ >= 0))

    assert(
      latency >= 0,
      "invalid generator with negative latency, " +
        "do you forgot to invoke GraphDone at the end of Dag construction?"
    )
  }

  final def inputWidths: Seq[Int] = inputTypes.map(_.bitWidth)

  final def outputWidths: Seq[Int] = outputTypes.map(_.bitWidth)

  def sizeIn: Int = inputWidths.length

  def sizeOut: Int = outputWidths.length

  def needNoControl: Boolean = inputFormat.period == 1 && outputFormat.period == 1

  def inputNoControl: FrameFormat = FrameFormat(Seq(0 until sizeIn))

  def outputNoControl: FrameFormat = FrameFormat(Seq(0 until sizeOut))

  def actualInTimes: Seq[Int] = inputTimes.getOrElse(inputTypes.map(_ => 0))

  def actualOutTimes: Seq[Int] = outputTimes.getOrElse(outputTypes.map(_ => 0))

  def period = {
    require(inputFormat.period == outputFormat.period)
    inputFormat.period
  }

  def getAutoName = {
    val fieldSymbols = typeOf[this.type].members.map(_.asTerm).filter(_.isCaseAccessor).toSeq.reverse
    val fieldNames = fieldSymbols.map(_.name)
    val instanceMirror: universe.InstanceMirror = mirror.reflect(this)
    val valuesAndNames = fieldSymbols.map(instanceMirror.reflectField).map(_.get).zip(fieldNames.map(_.toString)).toMap
    val fieldName = valuesAndNames.map { case (value, name) =>
      value match {
        case boolean: Boolean => if (boolean) name else s"not$name"
        case bigInt: BigInt => hashName(bigInt)
        case operatorType: OperatorType => className(operatorType)
        case _ => value.toString
      }
    }.mkString("_")
    logger.info(s"valAndName: ${valuesAndNames.mkString(" ")}")
    className(this) + "_" + fieldName
  }
}
