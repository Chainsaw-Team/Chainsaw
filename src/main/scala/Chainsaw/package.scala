
import com.mathworks.matlab.types
import com.mathworks.engine.MatlabEngine

import cc.redberry.rings.scaladsl.IntZ
import org.slf4j.LoggerFactory
import spinal.core._
import spinal.lib._

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt
import scala.reflect.ClassTag

import breeze.math._
import scala.language.implicitConversions

package object Chainsaw {

  /** --------
   * global run-time environment
   * -------- */
  val logger = LoggerFactory.getLogger("Chainsaw logger") // global logger
  var verbose = 0

  val naiveSet = mutable.Set[String]()

  def setAsNaive(generator: Any*) = naiveSet += generator.getClass.getSimpleName.replace("$", "")

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

  /** --------
   * scala type utils
   * -------- */
  implicit class IntUtil(int: Int) {
    def divideAndCeil(base: Int) = (int + base - 1) / base

    def nextMultiple(base: Int) = divideAndCeil(base) * base
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

    def takeLow(n: Int) = splitAt(n)._2

    def takeHigh(n: Int) = splitAt(width - n)._1

    def apply(range: Range) = {
      (value / Pow2(range.low)) % Pow2(range.length)
    }

    /** split the BigInt uniformly into n segments, low to high
     */
    def splitN(n: Int): Seq[BigInt] = {
      val padded = BitValue(value, width.nextMultiple(n))
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
      require(vec.length == that.length)
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

  def ChainsawSynth(gen: ChainsawGenerator, name: String, withRequirement: Boolean = false) = {
    val report = VivadoSynth(gen.implH, name)
    if (withRequirement) report.require(gen.utilEstimation, gen.fmaxEstimation)
    report
  }

  def ChainsawImpl(gen: ChainsawGenerator, name: String, withRequirement: Boolean = false) = {
    val report = VivadoImpl(gen.implH, name)
    if (withRequirement) report.require(gen.utilEstimation, gen.fmaxEstimation)
    report
  }

  /** --------
   * util functions
   * -------- */
  object Pow2 {
    def apply(exp: Int) = BigInt(1) << exp
  }

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
   * matlab utils
   * -------- */
  //  type MComplex = types.Complex
  lazy val matlabEngine = MatlabEngine.startMatlab()
  //
  //  /** implicit conversion from Matlab Complex to Breeze Complex
  //   */
  //  implicit def ComplexConversion(mcomplex: MComplex): Complex = Complex(mcomplex.real, mcomplex.imag)

  //  implicit class mcomplexConversion(mcomplex: MComplex){
  //    def toComplex = Complex(mcomplex.real, mcomplex.imag)
  //  }

}
