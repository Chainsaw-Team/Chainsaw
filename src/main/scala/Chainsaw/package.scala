
import Chainsaw.deprecated.ChainsawGenerator
import Chainsaw.xilinx.xilinxCDConfig
import cc.redberry.rings.scaladsl.IntZ
import com.mathworks.engine.MatlabEngine
import org.slf4j.LoggerFactory
import spinal.core._
import spinal.core.internals.PhaseContext
import spinal.core.sim.SpinalSimBackendSel
import spinal.lib._

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.math.BigInt
import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.util.Random
import spinal.core.internals._

package object Chainsaw {

  def inVirtualGlob[T](func: => T) = {
    val old = GlobalData.get

    val virtualGlob = new GlobalData(SpinalConfig())
    virtualGlob.phaseContext = new PhaseContext(SpinalConfig())
    GlobalData.set(virtualGlob)
    val ret = func

    GlobalData.set(old)
    ret
  }

  def inVirtualComponent[T](func: => T) = {
    inVirtualGlob {
      val com = new Module {
        val ret = func
      }
      com.ret
    }
  }


  // for numeric type calculation
  //  val virtualGlob = new GlobalData(SpinalConfig())
  //  virtualGlob.phaseContext = new PhaseContext(SpinalConfig())
  //  GlobalData.set(virtualGlob)

  /** --------
   * global run-time environment
   * -------- */
  val logger = LoggerFactory.getLogger("Chainsaw logger") // global logger
  var verbose = 0

  val naiveSet = mutable.Set[String]()

  def setAsNaive(generator: Any*) = naiveSet += generator.getClass.getSimpleName.replace("$", "")


  var allowSynthAndImpl = false // TODO: implement this by a config file
  var testFlopoco = false
  var testVhdl = false
  var atSimTime = true

  val dot = "■"
  val downArrow = "↓"
  /** --------
   * type def
   * -------- */
  type Metric = (Any, Any) => Boolean
  type FrameMetric = (Seq[Any], Seq[Any]) => Boolean

  /** --------
   * paths
   * -------- */
  val vivadoPath = new File("/tools/Xilinx/Vivado/2021.1/bin/vivado") // vivado executable path TODO: should be read from environment variables
  val quartusDir = new File("/tools/quartus/bin")
  val unisimDir = new File("src/main/resources/unisims")
  val genWorkspace = new File("genWorkspace")
  val simWorkspace = new File("simWorkspace")
  val synthWorkspace = new File("synthWorkspace")
  val cplexJarPath = new File("/opt/ibm/ILOG/CPLEX_Studio1210/cplex/lib/cplex.jar")
  val flopocoPath = new File("/home/ltr/flopoco/build/flopoco")
  val flopocoOutputDir = new File("src/main/resources/flopocoGenerated")
  val matlabScriptDir = new File("src/main/resources/matlabScripts")

  /** --------
   * scala type utils
   * -------- */
  implicit class IntUtil(int: Int) {
    def divideAndCeil(base: Int) = (int + base - 1) / base

    def nextMultipleOf(base: Int) = divideAndCeil(base) * base

    def divideToBlock(blockCount: Int) = {
      val fullLength = divideAndCeil(blockCount)
      val diff = fullLength * blockCount - int
      Seq.fill(blockCount - 1)(fullLength) :+ fullLength - diff
    }

    def divideToChannel(channelCount: Int) = {
      val fullLength = divideAndCeil(channelCount)
      val diff = fullLength * channelCount - int
      Seq.fill(channelCount - diff)(fullLength) ++ Seq.fill(diff)(fullLength - 1)
    }
  }

  implicit class StringUtil(s: String) {

    // complement version of method padTo(padToRight)
    def padToLeft(len: Int, elem: Char) = s.reverse.padTo(len, elem).reverse

    def repeat(times: Int) = Seq.fill(times)(s).reduce(_ + _)
  }

  implicit class seqUtil[T: ClassTag](seq: Seq[T]) {
    def prevAndNext[TOut](f: ((T, T)) => TOut) = seq.init.zip(seq.tail).map(f)

    def padToLeft(len: Int, elem: T) = seq.reverse.padTo(len, elem).reverse
  }

  case class BitValue(value: BigInt, width: Int) {

    /** works the same as SpinalHDL splitAt
     *
     * @example 10100.split(3) = (10,100)
     */
    def splitAt(lowWidth: Int): (BigInt, BigInt) = {
      require(value >= 0, s"$value")
      val base = BigInt(1) << lowWidth
      (value >> lowWidth, value % base)
    }

    def toBinaryBigInt = if (value >= 0) value else (BigInt(1) << width) + value

    def takeLow(n: Int) = splitAt(n)._2

    def takeHigh(n: Int) = splitAt(width - n)._1

    def apply(range: Range) = {
      (value / Pow2(range.low)) % Pow2(range.length)
    }

    /** split the BigInt uniformly into n segments, low to high
     */
    def splitN(n: Int): Seq[BigInt] = {
      val padded = BitValue(value, width.nextMultipleOf(n))
      val segmentWidth = width.divideAndCeil(n)
      val segments = ArrayBuffer[BigInt]()
      var current = padded
      (0 until n - 1).foreach { i =>
        val (high, low) = current.splitAt(segmentWidth)
        segments += low
        current = high.toBitValue(segmentWidth * (n - i - 1))
      }
      segments += current.value
      segments
    }

    def ##(that: BitValue) = (this.value << that.width) + that.value
  }

  // TODO: make BigInt behaves just like Bits/UInt
  implicit class BigIntUtil(bi: BigInt) {
    def toBitValue(width: Int = -1) = {
      if (width == -1) BitValue(bi, bi.bitLength)
      else BitValue(bi, width)
    }
  }

  /** --------
   * spinal type utils
   * -------- */

  implicit class MemUtil(mem: Mem[_]) {
    def setAsBlockRam() = mem.addAttribute("ram_style", "block")

    def setAsUltraRam() = mem.addAttribute("ram_style", "ultra")
  }

  // extension of Data
  implicit class DataUtil[T <: Data](data: T) {
    def d(cycle: Int = 1): T = Delay(data, cycle)

    def d(cycle: Int, init: T): T = Delay(data, cycle, init = init)
  }

  // extension of Bool
  implicit class BoolUtil(data: Bool) {
    // drive a flag which is initially unset
    def validAfter(cycle: Int): Bool = Delay(data, cycle, init = False)
  }

  implicit class VecUtil[T <: Data](vec: Vec[T]) {
    def :=(that: Seq[T]): Unit = {
      require(vec.length == that.length, s"vec length ${that.length} -> ${vec.length}")
      vec.zip(that).foreach { case (port, data) => port := data }
    }

    def vecShiftWrapper(bitsShift: UInt => Bits, that: UInt): Vec[T] = {
      val ret = cloneOf(vec)
      val shiftedBits: Bits = bitsShift((that * widthOf(vec.dataType)).resize(log2Up(widthOf(vec.asBits))))
      ret.assignFromBits(shiftedBits)
      ret
    }

    val bits = vec.asBits

    def rotateLeft(that: Int): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateLeft(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateRight(that: Int): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)

    def rotateRight(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)
  }

  type ChainsawFlow = Flow[Fragment[Vec[AFix]]]

  implicit class ChainsawFlowUtil(flow: ChainsawFlow) {
    def mapFragment(func: Seq[AFix] => Seq[AFix], latency: Int = 0): ChainsawFlow = {
      val temp = func(flow.fragment)
      val newFragment = Vec(temp)
      val ret = new Flow(new Fragment(newFragment))
      ret.fragment := newFragment
      ret.valid := flow.valid.validAfter(latency)
      ret.last := flow.last.validAfter(latency)
      ret
    }

    def >>(that: ChainsawBaseModule): Unit = that.flowIn := flow

    def foreach(func: BaseType => Unit): Unit = {
      flow.fragment.map(_.raw).foreach(func)
      func(flow.valid)
      func(flow.last)
    }
  }

  /** --------
   * Flows
   * -------- */

  import xilinx._

  def ChainsawGen(gen: ChainsawGenerator, name: String) = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC),
      targetDirectory = genWorkspace.getAbsolutePath + "/",
      oneFilePerComponent = true)
      .generateVerilog(gen.implH.setDefinitionName(name))
  }

  def ChainsawSynthOld(gen: ChainsawGenerator, name: String, withRequirement: Boolean = false) = {
    // TODO: with requirement, + ffs before and after the component, - ffs before comparison
    val report = VivadoSynth(gen.implH, name)
    if (withRequirement) report.require(gen.utilEstimation, gen.fmaxEstimation)
    report
  }

  def ChainsawImplOld(gen: ChainsawGenerator, name: String, withRequirement: Boolean = false) = {
    val report = VivadoImpl(gen.implH, name)
    if (withRequirement) report.require(gen.utilEstimation.toRequirement, gen.fmaxEstimation)
    report
  }

  def ChainsawSynth(gen: ChainsawBaseGenerator, withRequirement: Boolean = false) = {
    // TODO: with requirement, + ffs before and after the component, - ffs before comparison
    atSimTime = false
    val report = VivadoSynth(gen.implH, gen.name, ChainsawSpinalConfig(gen))
    if (withRequirement) report.require(gen.vivadoUtilEstimation.toRequirement, gen.fmaxEstimation)
    report
  }

  def ChainsawImpl(gen: ChainsawBaseGenerator, withRequirement: Boolean = false) = {
    atSimTime = false
    val report = VivadoImpl(gen.implH, gen.name, ChainsawSpinalConfig(gen))
    if (withRequirement) report.require(gen.vivadoUtilEstimation.toRequirement, gen.fmaxEstimation)
    report
  }

  /** --------
   * util functions
   * -------- */
  object Pow2 {
    def apply(exp: Int) = BigInt(1) << exp
  }

  def nextPow2(n: Int): BigInt = BigInt(1) << log2Up(n)

  def randBigInt(width: Int) = BigInt(width, Random)

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = {
    val (p, q) = if (a >= b) (a, b) else (b, a)
    if (q == 0) p
    else gcd(q, p % q)
  }

  def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)

  implicit class intzUti(intz: IntZ) {
    def toBigInt = BigInt(intz.toByteArray)
  }

  /** --------
   * to getUniqueName
   * -------- */

  val mirror: universe.Mirror = runtimeMirror(getClass.getClassLoader)

  def className(any: Any) = any.getClass.getSimpleName.replace("$", "")

  def hashName(any: Any) = any.hashCode().toString.replace("-", "N")

  @deprecated
  def getAutoName[T: TypeTag](obj: T)(implicit tag: ClassTag[T]) = {
    val fieldSymbols = typeOf[T].members
      .filter(_.isMethod)
      .map(_.asTerm)
      .filter(_.isCaseAccessor)
      .toSeq.reverse
    val fieldNames = fieldSymbols.map(_.name)
    val instanceMirror: universe.InstanceMirror = mirror.reflect(obj)
    val valuesAndNames = fieldNames.map(_.toString).zip(fieldSymbols.map(instanceMirror.reflectField).map(_.get)).sortBy(_._1)

    def getFieldName(name: String, value: Any) = {
      value match {
        case boolean: Boolean => if (boolean) name.trim else s"not${name.trim}"
        case chainsawEnum: ChainsawEnum => className(chainsawEnum)
        case sim: SpinalSimBackendSel => className(sim)
        case bigInt: BigInt => hashName(bigInt).replace('-', 'N')
        case seq: Seq[_] => hashName(seq)
        case double: Double => hashName(double)
        case int: Double => int.toString.replace("-", "N")
        case numericType: NumericType => s"${numericType.integral}_${numericType.fractional}".replace('-', 'N') // TODO: remove old numericType
        case numericType: NumericType => s"${numericType.integral}_${numericType.fractional}".replace('-', 'N')
        case hertzNumber: HertzNumber => (hertzNumber / 1e6).toInt
        case operatorType: OperatorType => className(operatorType)
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
    className(obj) + "_" + fieldName
  }

  /** --------
   * matlab utils
   * -------- */
  lazy val matlabEngine = MatlabEngine.startMatlab()
}
