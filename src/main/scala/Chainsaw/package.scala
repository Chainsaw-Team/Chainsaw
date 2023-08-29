import cc.redberry.rings.scaladsl.IntZ
import com.mathworks.engine.MatlabEngine
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._
import spinal.core.internals.PhaseContext
import spinal.core.sim._
import spinal.lib._

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.reflect.ClassTag // for more simulation
import Chainsaw.NumericExt._
import Chainsaw.edaFlow._

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

  /** -------- global run-time environment
    * --------
    */

  // loading configs
  import org.yaml.snakeyaml.Yaml

  import scala.io.Source

  val yaml                 = new Yaml()
  private val configSource = Source.fromFile("config.yaml")
  private val configString = configSource.getLines().mkString("\n")
  private val configs =
    yaml.load(configString).asInstanceOf[java.util.LinkedHashMap[String, Any]]

  val hasVivado: Boolean  = sys.env.contains("VIVADO")
  val hasQuartus: Boolean = sys.env.contains("QUARTUS")
  val hasYosys: Boolean   = sys.env.contains("YOSYS")
  val hasPython: Boolean  = sys.env.contains("PYTHON")
  val hasFlopoco: Boolean = sys.env.contains("FLOPOCO")

  val allowSynth: Boolean = configs.get("allowSynth").asInstanceOf[Boolean] && hasVivado
  val allowImpl: Boolean  = configs.get("allowImpl").asInstanceOf[Boolean] && hasVivado

  val deviceFamilyList = Map(
    "cyclone v"  -> CycloneV,
    "ultrascale" -> UltraScale,
    "7 series"   -> Series7
  )

  val targetDeviceFamily = {
    val name = configs.get("targetDeviceFamily").asInstanceOf[String]
    deviceFamilyList.getOrElse(name.toLowerCase, Generic)
  }

  val dspStrict: Boolean = configs.get("dspStrict").asInstanceOf[Boolean]
  val verbose: Int       = configs.get("verbose").asInstanceOf[Int]
  configSource.close()

  // global data
  val logger: Logger =
    LoggerFactory.getLogger("Chainsaw logger") // global logger
  var atSimTime = true // indicating current task(sim/synth or impl)

  val naiveSet: mutable.Set[String] =
    mutable.Set[
      String
    ]() // list of generators which should be implemented by its naive version
  def setAsNaive(
      generator: Any*
  ): naiveSet.type = // add a generator to the naiveSet
    naiveSet += generator.getClass.getSimpleName.replace("$", "")

  var testFlopoco = false
  var testVhdl    = false

  var testCaseIndex = 0

  // TODO: better dots
  val positiveDot   = "+"
  val complementDot = "-"
  val downArrow     = "↓"

  /** -------- type def
    * --------
    */
  type Metric      = (Any, Any)           => Boolean
  type FrameMetric = (Seq[Any], Seq[Any]) => Boolean

  /** -------- paths
    * --------
    */

  // outside Chainsaw
  val vivadoPath =
    new File(sys.env.getOrElse("VIVADO", "")) // vivado executable path
  val vitisPath =
    new File(sys.env.getOrElse("VITIS", "")) // vitis executable path
  val flopocoPath = new File(sys.env.getOrElse("FLOPOCO", ""))
  val quartusPath = new File(sys.env.getOrElse("QUARTUS", ""))
  val quartusDir  = quartusPath.getParentFile                 // quartus executable dir
  val pythonPath  = new File(sys.env.getOrElse("PYTHON", "")) // python executable

  // inside Chainsaw
  val unisimDir =
    new File("src/main/resources/unisims") // for Xilinx primitives
  val matlabScriptDir = new File("src/main/resources/matlabScripts")

  val genWorkspace   = new File("genWorkspace")   // RTL
  val simWorkspace   = new File("simWorkspace")   // waveform
  val synthWorkspace = new File("synthWorkspace") // log & checkpoint

  val flopocoOutputDir = new File("src/main/resources/flopocoGenerated")
  val dagOutputDir     = new File("src/main/resources/dfgGenerated")

  /** -------- scala type utils
    * --------
    */
  implicit class IntUtil(int: Int) {
    def divideAndCeil(base: Int): Int = (int + base - 1) / base

    def nextMultipleOf(base: Int): Int = divideAndCeil(base) * base

    def divideToBlock(blockCount: Int) = {
      val fullLength = divideAndCeil(blockCount)
      val diff       = fullLength * blockCount - int
      Seq.fill(blockCount - 1)(fullLength) :+ fullLength - diff
    }

    def divideToChannel(channelCount: Int) = {
      val fullLength = divideAndCeil(channelCount)
      val diff       = fullLength * channelCount - int
      Seq.fill(channelCount - diff)(fullLength) ++ Seq.fill(diff)(fullLength - 1)
    }
  }

  implicit class StringUtil(s: String) {

    // complement version of method padTo(padToRight)
    def padToLeft(len: Int, elem: Char): String =
      s.reverse.padTo(len, elem).reverse

    def repeat(times: Int) = Seq.fill(times)(s).reduce(_ + _)

    def firstBeforeChar(char: Char): String = s.split(char).head
  }

  implicit class seqUtil[T: ClassTag](seq: Seq[T]) {
    // TODO: use sliding(2) instead
    def prevAndNext[TOut](f: ((T, T)) => TOut): Seq[TOut] =
      seq.init.zip(seq.tail).map(f)

    def padToLeft(len: Int, elem: T): Seq[T] =
      seq.reverse.padTo(len, elem).reverse
  }

  /** to manipulate a BigInt as Bits, you need a BitValue first, as BigInt has no width information
    */
  implicit class BigIntUtil(bi: BigInt) {
    def toBitValue(width: Int = -1) = {
      if (width == -1) BitValue(bi, bi.bitLength)
      else BitValue(bi, width)
    }
  }

  /** -------- spinal type utils
    * --------
    */
  implicit class MemUtil(mem: Mem[_]) {
    def setAsBlockRam() = mem.addAttribute("ram_style", "block")

    def setAsUltraRam() = mem.addAttribute("ram_style", "ultra")
  }

  // extension of Data
  implicit class DataUtil[T <: Data](data: T) {
    def d(cycle: Int = 1): T = Delay(data, cycle) // delay

    def d(cycle: Int, init: T): T =
      Delay(data, cycle, init = init) // delay with initValue

    def changed: Bool = data.d() =/= data
  }

  // extension of Bool
  implicit class BoolUtil(data: Bool) {
    // drive a flag which is initially unset
    def validAfter(cycle: Int): Bool = Delay(data, cycle, init = False)
  }

  implicit class VecUtil[T <: Data](vec: Vec[T]) {
    def :=(that: Seq[T]): Unit = {
      require(
        vec.length == that.length,
        s"vec length ${that.length} -> ${vec.length}"
      )
      vec.zip(that).foreach { case (port, data) => port := data }
    }

    // allow vec-level shift
    def vecShiftWrapper(bitsShift: UInt => Bits, that: UInt): Vec[T] = {
      val ret = cloneOf(vec)
      val shiftedBits: Bits = bitsShift(
        (that * widthOf(vec.dataType)).resize(log2Up(widthOf(vec.asBits)))
      )
      ret.assignFromBits(shiftedBits)
      ret
    }

    val bits = vec.asBits

    def rotateLeft(that: Int): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateLeft(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateRight(that: Int): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)

    def rotateRight(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)

    def groupByChannel(channelCount: Int) = vec.zipWithIndex
      .groupBy { case (_, i) => i % channelCount }
      .map(_._2.map(_._1))
      .map(Vec(_))
      .toSeq
  }

  // for easy connection between ChainsawModules
  type ChainsawVec  = Seq[AFix]
  type ChainsawFlow = Flow[Fragment[Vec[AFix]]]

  object ChainsawFlow {
    def apply(payload: Seq[AFix], valid: Bool, last: Bool): ChainsawFlow = {
      val fragment = Vec(payload)
      val ret      = new Flow(new Fragment(fragment))
      ret.fragment := fragment
      ret.valid    := valid
      ret.last     := last
      ret
    }

    def apply[T <: Data](hardType: HardType[Vec[T]]) = new Flow(new Fragment(hardType()))
  }

  implicit class ChainsawFlowUtil(flow: ChainsawFlow) {

    /** replace fragment of current flow, pipeline valid & last when needed
      */
    def mapFragment(
        func: Seq[AFix] => Seq[AFix],
        latency: Int = 0
    ): ChainsawFlow = {
      val temp        = func(flow.fragment)
      val newFragment = Vec(temp)
      val ret         = new Flow(new Fragment(newFragment))
      ret.fragment := newFragment
      ret.valid    := flow.valid.validAfter(latency)
      ret.last     := flow.last.validAfter(latency)
      ret
    }

    def pipe(latency: Int): ChainsawFlow = flow.mapFragment(vec => Vec(vec).d(latency), latency)

    def >>(that: ChainsawBaseModule): Flow[Fragment[Vec[AFix]]] = {
      that.flowIn := flow
      that.flowOut
    }

    def >>(gen: ChainsawBaseGenerator): Flow[Fragment[Vec[AFix]]] = this >> gen.implH

    def >>(that: ChainsawBaseModule with DynamicModule, control: Vec[AFix]): Flow[Fragment[Vec[AFix]]] = {
      that.flowIn    := flow
      that.controlIn := control
      that.flowOut
    }

    def >>(gen: ChainsawBaseGenerator with Dynamic, control: Vec[AFix]): Flow[Fragment[Vec[AFix]]] = {
      this >> (gen.implH.asInstanceOf[ChainsawBaseModule with DynamicModule], control)
    }

    def >>>(
        that: ChainsawBaseGenerator
    ): Flow[Fragment[Vec[AFix]]] = {
      assert(that.inputTypes.length == 1)
      val retFlows = this.split.map(_ >> (that))
      retFlows.head.mapFragment(_ => retFlows.flatMap(_.fragment))
    }

    def >>>(
        that: ChainsawBaseGenerator with Dynamic,
        control: Vec[AFix]
    ): Flow[Fragment[Vec[AFix]]] = {
      assert(that.inputTypes.length == 1)
      val retFlows = this.split.map(_ >> (that, control))
      retFlows.head.mapFragment(_ => retFlows.flatMap(_.fragment))
    }

    def getPhase(id: Int) = flow.mapFragment(vec => Seq(vec(id)))

    def foreach(func: BaseType => Unit): Unit = {
      flow.fragment.map(_.raw).foreach(func)
      func(flow.valid)
      func(flow.last)
    }

    def withLast(last: Bool) = {
      val ret = new Flow(new Fragment(flow.fragment))
      ret.fragment := flow.fragment
      ret.valid    := flow.valid
      ret.last     := last
      ret
    }

    def withValid(valid: Bool) = {
      val ret = new Flow(new Fragment(flow.fragment))
      ret.fragment := flow.fragment
      ret.valid    := valid
      ret.last     := flow.last
      ret
    }

    def exportAs(name: String)(implicit monitoredFlows: ArrayBuffer[ChainsawFlow]) = {
      monitoredFlows += flow
      flow.setName(name)
      flow.simPublic()
    }

    def fixTo(af: AFix) = flow.mapFragment(_.fixTo(af))

    def split: Seq[ChainsawFlow] = flow.fragment.map(ele => flow.mapFragment(_ => Vec(ele)))

    def dataType = {
      val ret = flow.fragment.head.numericType
      assert(
        flow.fragment.forall(_.numericType == ret),
        s"head = ${ret}, others = ${flow.fragment.map(_.numericType).mkString(" ")}"
      )
      ret
    }

    def toRealAndImag = (flow.mapFragment(_.toComplexFix.map(_.real)), flow.mapFragment(_.toComplexFix.map(_.imag)))

    def real = toRealAndImag._1
    def imag = toRealAndImag._2

    def exportAsComplex(name: String)(implicit monitoredFlows: ArrayBuffer[ChainsawFlow]) = {
      this.real.exportAs(s"name${real}")
      this.imag.exportAs(s"name${imag}")
    }

    def zipWithFlows(chainsawFlows: ChainsawFlow*): ChainsawFlow = {
      val fragment: Seq[AFix] = (flow +: chainsawFlows).flatMap(_.fragment)
      flow.mapFragment(_ => fragment)
    }

    def subDivideIn(slicesCount: SlicesCount) = {
      assert(flow.fragment.length % slicesCount.value == 0)
      val fragments: Seq[IndexedSeq[AFix]] = flow.fragment.grouped(flow.fragment.length / slicesCount.value).toSeq
      fragments.map(fragment => flow.mapFragment(_ => fragment))
    }

    def zipAndDivide(slicesCount: SlicesCount, chainsawFlows: ChainsawFlow*): Seq[ChainsawFlow] = {
      val allFlows = flow +: chainsawFlows
      assert(allFlows.forall(flow => flow.fragment.length % slicesCount.value == 0))
      val matrix = allFlows.map(_.subDivideIn(slicesCount))
      matrix.transpose.map(col => col.head.zipWithFlows(col.tail: _*))
    }

  }

  def fromRealAndImag(real: ChainsawFlow, imag: ChainsawFlow): ChainsawFlow = {
    val fragments = real.fragment.zip(imag.fragment).flatMap { case (r, i) => Seq(r, i) }
    real.mapFragment(_ => fragments)
  }

  /** -------- Flows
    * --------
    */

  import xilinx._

  /** generators in naiveList are set as naive in this box
    */
  def ChainsawSimBox(naiveList: Seq[String])(test: => Unit): Unit = {
    naiveSet ++= naiveList
    test
    naiveSet.clear() // clear naiveSet after test
  }

  def ChainsawEdaFlow(
      gen: ChainsawBaseGenerator,
      edaFlowType: EdaFlowType,
      requirementStrategy: UtilRequirementStrategy
  ) = {
    atSimTime = false // set environment
    try {
      val report = edaFlowType match {
        case SYNTH =>
          VivadoSynth(gen.implH, gen.name, ChainsawSpinalConfig(gen))
        case IMPL => VivadoImpl(gen.implH, gen.name, ChainsawSpinalConfig(gen))
      }

      report.requireUtil(gen.vivadoUtilEstimation, requirementStrategy)
      report.requireFmax(gen.fmaxEstimation)
      report
    } finally atSimTime = true // reset environment
  }

  def ChainsawSynth(
      gen: ChainsawBaseGenerator,
      requirementStrategy: UtilRequirementStrategy = DefaultRequirement
  ) = ChainsawEdaFlow(gen, SYNTH, requirementStrategy)

  def ChainsawImpl(
      gen: ChainsawBaseGenerator,
      requirementStrategy: UtilRequirementStrategy = DefaultRequirement
  ) = ChainsawEdaFlow(gen, IMPL, requirementStrategy)

  /** -------- util functions
    * --------
    */
  def pow2(exp: Int) = BigInt(1) << exp

  def nextPow2(n: Int): BigInt = BigInt(1) << log2Up(n)

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = {
    val (p, q) = if (a >= b) (a, b) else (b, a)
    if (q == 0) p
    else gcd(q, p % q)
  }

  def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)

  /** -------- to getUniqueName
    * --------
    */
  // get name of a class/object
  def className(any: Any) = any.getClass.getSimpleName.replace("$", "")
  // get name of a unique "configuration"
  def hashName(any: Any) = any.hashCode().toString.replace("-", "N")

  /** -------- rings utils
    * --------
    */
  implicit class intzUti(intz: IntZ) {
    def toBigInt = BigInt(intz.toByteArray)
  }

  /** -------- matlab utils
    * --------
    */
  lazy val matlabEngine = MatlabEngine.startMatlab()
}
