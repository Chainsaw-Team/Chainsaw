package Chainsaw.dsp

import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.intel.QuartusFlow
import Chainsaw.io.pythonIo._
import Chainsaw.edaFlow.vivado._
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import org.jgrapht._
import org.jgrapht.graph._
import spinal.core._

import scala.collection.JavaConverters._

case class MacConfig(
    dataType: NumericType,
    coeffType: NumericType
) {
  def prodType = dataType * coeffType
}
trait MacNode {
  def delay: Int

  def impl(vec: ChainsawVec)(implicit config: MacConfig): ChainsawVec

  def placeHolder(implicit config: MacConfig): ChainsawVec

  def slow(factor: Int): Seq[MacNode]

  override def toString = this.getClass.getSimpleName
}

class Input extends MacNode {
  override def delay: Int                                                      = 0
  override def impl(vec: ChainsawVec)(implicit config: MacConfig): ChainsawVec = vec
  override def placeHolder(implicit config: MacConfig): ChainsawVec            = Seq(config.dataType())
  override def slow(factor: Int): Seq[MacNode]                                 = this +: Seq.fill(factor - 1)(new Input)
}
class Output extends MacNode {
  override def delay: Int                                                      = 0
  override def impl(vec: ChainsawVec)(implicit config: MacConfig): ChainsawVec = vec
  override def placeHolder(implicit config: MacConfig): ChainsawVec            = Seq(config.prodType())
  override def slow(factor: Int): Seq[MacNode] = this +: Seq.fill(factor - 1)(new Output)
}

class Scale(val coeff: Double) extends MacNode {
  override def delay: Int = 1

  // TODO: avoid big mult using SInt
  override def impl(vec: ChainsawVec)(implicit config: MacConfig): ChainsawVec =
    vec
      .map(_.fixTo(config.dataType()).asSInt() * config.coeffType.fromConstant(coeff).asSInt().d())
      .map(si => config.prodType.fromBits(si.asBits))

  override def slow(factor: Int): Seq[MacNode]                      = this +: Seq.fill(factor - 1)(new Scale(coeff))
  override def placeHolder(implicit config: MacConfig): ChainsawVec = Seq(config.prodType())
}

class Sum extends MacNode {
  override def delay: Int                                                      = 0
  override def impl(vec: ChainsawVec)(implicit config: MacConfig): ChainsawVec = Vec(vec.reduce(_ +| _))
  override def slow(factor: Int): Seq[MacNode]                                 = this +: Seq.fill(factor - 1)(new Sum)
  override def placeHolder(implicit config: MacConfig): ChainsawVec            = Seq(config.prodType())
}

class PreAdd extends MacNode {
  override def delay: Int                                                      = 0
  override def impl(vec: ChainsawVec)(implicit config: MacConfig): ChainsawVec = Vec(vec.reduce(_ +| _))
  override def slow(factor: Int): Seq[MacNode]                      = this +: Seq.fill(factor - 1)(new PreAdd)
  override def placeHolder(implicit config: MacConfig): ChainsawVec = Seq(config.dataType())
}

class MacEdge
class MacGraph(implicit config: MacConfig) extends DirectedWeightedPseudograph[MacNode, MacEdge](classOf[MacEdge]) {

  def addEdgeWithWeight(src: MacNode, target: MacNode, weight: Int) = {
    val edge = new MacEdge
    this.addEdge(src, target, edge)
    this.setEdgeWeight(edge, weight)
  }

  def getInputs = vertexSet().asScala.filter(_.isInstanceOf[Input]).toSeq

  def getOutputs = vertexSet().asScala.filter(_.isInstanceOf[Output]).toSeq

  // TODO: retiming
  def unfold(factor: Int) = {
    val orderedNodes = vertexSet().asScala.toSeq

    val nodeMap: Map[MacNode, Seq[MacNode]] = orderedNodes.map { node => // node -> its duplicates
      val duplicates = node.slow(factor)
      duplicates.foreach(this.addVertex)
      node -> duplicates
    }.toMap

    val edges = edgeSet().asScala.toSeq // keep it
    edges.foreach { edge =>
      val srcDuplicates    = nodeMap(this.getEdgeSource(edge))
      val targetDuplicates = nodeMap(this.getEdgeTarget(edge))
      (0 until factor).foreach { i =>
        val newEdge = new MacEdge
        val w       = getEdgeWeight(edge).toInt
        this.addEdge(srcDuplicates(i), targetDuplicates((w + i) % factor), newEdge)
        this.setEdgeWeight(newEdge, (w + i) / factor)
      }
      removeEdge(edge)
    }
  }

  def retime() = {
    def getRetimingValue = {
      val maximumLatency          = 500
      implicit val model: MPModel = MPModel(SolverLib.oJSolver)

      // declare vertices as variables
      val vertices = vertexSet().asScala.toSeq
      val variableMap: Map[MacNode, MPFloatVar] = vertices.zipWithIndex.map { case (vertex, i) =>
        vertex -> MPFloatVar(s"r$i", 0, maximumLatency)
      }.toMap

      // the all-in-one target function
      val ins  = getInputs
      val outs = getOutputs.toSeq
      add(variableMap(ins.head) := 0) // set value 0 for the first input port
      val coeffs = outs.map(_ => 1.0) ++ ins.map(_ => -1.0)
      val expr = (outs ++ ins)
        .zip(coeffs)
        .map { case (v, coeff) => variableMap(v) * coeff }
        .reduce(_ + _) // \Sigma outs - \Sigma ins

      minimize(expr)

      // constraint 1: inputs/outputs are aligned
      ins.prevAndNext { case (prev, next) => add(variableMap(next) - variableMap(prev) := 0) }
      outs.prevAndNext { case (prev, next) => add(variableMap(next) - variableMap(prev) := 0) }
      // constraint 2: component latency & non-negative edge, regrading every component vertex as the input port
      edgeSet().asScala.foreach { e =>
        val sourceVar = variableMap(getEdgeSource(e))
        val targetVar = variableMap(getEdgeTarget(e))
        if (!ins.contains(getEdgeSource(e))) {
          val opLatency   = getEdgeSource(e).delay
          val edgeLatency = getEdgeWeight(e)
          add(targetVar - sourceVar + edgeLatency >:= opLatency)
        }
      }

      // solve the LP problem
      start()

      // rounding, toInt is equivalent to ceil, which is not appropriate
      import scala.math.round
      val minValue = variableMap.values.map(_.value.get).min
      val solution = vertices.map { v =>
        val variable = variableMap(v)
        v -> round(variable.value.getOrElse(-1.0) - minValue).toInt
      }.toMap
      if (verbose >= 1)
        logger.info(
          s"solution:\n\t ${vertices.map(v => s"$v -> ${solution(v)}").mkString("\n\t")}" +
            s"\n\tsolution latency = ${solution(outs.head) - solution(ins.head)}"
        )
      release()

      solution
    }

    val retimingValue = getRetimingValue
    edgeSet().asScala.foreach { e =>
      val inc = retimingValue(getEdgeTarget(e)) - retimingValue(getEdgeSource(e))
      setEdgeWeight(e, getEdgeWeight(e).toInt + inc)
    }

  }

  def connect(validIn: Bool, input: ChainsawVec, output: ChainsawVec)(implicit config: MacConfig): Unit = {
    assert(input.length == getInputs.length)
    assert(output.length == getOutputs.length)

    val signalMap: Map[MacNode, Vec[AFix]] = vertexSet().asScala.map(v => v -> Vec(v.placeHolder)).toMap
    signalMap.foreach { case (node, af) => println(s"$node -> ${af.head.numericType}") }

    getInputs.zip(input).foreach { case (node, af) => signalMap(node) := Mux(validIn, Vec(af), Vec(af).getZero) }

    vertexSet().asScala.foreach { v =>
      val srcs = Graphs.predecessorListOf(this, v).asScala
      if (srcs.nonEmpty) {
        val inputs = srcs.map { src =>
          val signal = signalMap(src)
          val init   = false
          if (init) signal.d(getEdgeWeight(getEdge(src, v)).toInt, signal.getZero)
          else signal.d(getEdgeWeight(getEdge(src, v)).toInt)
        }
        val ret = Vec(v.impl(inputs.map(_.toSeq).reduce(_ ++ _)))
        if (v.isInstanceOf[Output]) signalMap(v) := Mux(validIn, ret, signalMap(v).getZero)
        else signalMap(v)                        := ret
      } else if (!v.isInstanceOf[Input]) logger.warn(s"node $v has no driver")
    }

    getOutputs.zip(output).foreach { case (node, af) => af := signalMap(node).head }

  }
}

case class FilterByMag(b: Seq[Double], a: Seq[Double], parallel: Int, widthConfig: MacConfig)
    extends MacGraph()(widthConfig)
    with ChainsawInfiniteGenerator
    with FixedLatency {

  implicit val refConfig: MacConfig = widthConfig

  assert(a.head == 1.0)
  if (a.length > 1) assert(a.length == b.length)
  val tap       = b.length
  val half      = tap.divideAndCeil(2)
  val symmetric = b.take(half).equals(b.takeRight(half).reverse) && a.length == 1
//  val symmetric = false
  if (symmetric) println("your filter is a symmetric FIR")

  val x  = new Input
  val y  = new Output
  val as = a.tail.map(coeff => new Scale(-coeff))
  val bs = (if (symmetric) b.take(half) else b).map(new Scale(_))
  (as ++ bs :+ x :+ y) foreach addVertex

  if (a.length > 1) { // Direct-form I IIR Implementation
    val sums = Seq.fill(tap)(new Sum)
    sums foreach addVertex
    bs.zipWithIndex.foreach { case (scale, i) => addEdgeWithWeight(x, scale, i) }
    bs.zip(sums).foreach { case (scale, add) => addEdgeWithWeight(scale, add, 0) }
    as.zip(sums.tail).foreach { case (scale, add) => addEdgeWithWeight(scale, add, 0) }
    as.zipWithIndex.foreach { case (scale, i) => addEdgeWithWeight(sums.head, scale, i + 1) }
    sums.reverse.sliding(2).foreach { case Seq(prev, next) => addEdgeWithWeight(prev, next, 0) }
    addEdgeWithWeight(sums.head, y, 0)
  } else { // Systolic FIR implementation

    if (!symmetric) {
      val sums = Seq.fill(tap)(new Sum)
      sums foreach addVertex
      bs.zipWithIndex.foreach { case (scale, i) => addEdgeWithWeight(x, scale, i * 2 + 1) }
      bs.zip(sums).foreach { case (scale, sum) => addEdgeWithWeight(scale, sum, 1) }
      sums.sliding(2).foreach { case Seq(prev, next) => addEdgeWithWeight(prev, next, 1) }
      addEdgeWithWeight(sums.last, y, 1)
    } else {
      val preAdds = Seq.fill(half)(new PreAdd)
      val sums    = Seq.fill(half)(new Sum)
      (preAdds ++ sums) foreach addVertex
      preAdds.zipWithIndex.foreach { case (preAdd, i) =>
        addEdgeWithWeight(x, preAdd, (i + 1) * 2)
        addEdgeWithWeight(x, preAdd, tap + 1)
      }
      preAdds.zip(bs).foreach { case (sum, scale) => addEdgeWithWeight(sum, scale, 1) }
      bs.zip(sums).foreach { case (scale, sum) => addEdgeWithWeight(scale, sum, 1) }
      sums.sliding(2).foreach { case Seq(prev, next) => addEdgeWithWeight(prev, next, 1) }

      addEdgeWithWeight(sums.last, y, 1)
      edgeSet().asScala.foreach(e => println(s"${getEdgeSource(e)} -> ${getEdgeTarget(e)} = ${getEdgeWeight(e)}"))
    }
  }

  this.unfold(parallel)
  this.retime()

  override def implH: ChainsawInfiniteModule = new ChainsawInfiniteModule(this) {
    connect(validIn, dataIn, dataOut)
    lastOut := lastIn
  }

  override def implNaiveH: Option[ChainsawInfiniteModule] = ???

  override def name: String                     = s"FilterByMag_${b.hashCode().abs}_${a.hashCode().abs}"
  override def vivadoUtilEstimation: VivadoUtil = ???
  override def fmaxEstimation: HertzNumber      = ???
  override def inputTypes: Seq[NumericType]     = Seq.fill(parallel)(widthConfig.dataType)
  override def outputTypes: Seq[NumericType]    = Seq.fill(parallel)(widthConfig.prodType)
  override def impl(testCase: TestCase): Seq[BigDecimal] =
    runPythonModel("dsp", "lfilter", testCase.data, Some(testCase.control), Some(this))
  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean =
    corrMetric(yours, golden, threshold = 0.9)
  override def testCases: Seq[TestCase] = Seq.fill(10)(TestCase(randomDataSequence(1000).map(_ * 0.5)))
  override def latency(): Int           = 0
  override def resetCycle: Int          = 100
}

object FilterByMag {
  def main(args: Array[String]): Unit = {

    implicit val config: MacConfig = MacConfig(
      dataType  = NumericType.SFix(0, 17),
      coeffType = NumericType.SFix(0, 17)
    )

    val b  = Seq(0.0214, 0.1070, 0.2140, 0.2140, 0.1070, 0.0214)
    val a0 = Seq(1.0000, -4.1873, 7.0697, -6.0100, 2.5704, -0.4422)
    val a1 = Seq(1.0000)

    // FIXME: dynamic precision for as and bs
//    val iirGen = FilterByMag(b.take(2), a0.take(2), parallel = 1, config)
//    ChainsawSpinalConfig(iirGen).generateVerilog(iirGen.implH)
//    ChainsawTest(
//      testName = "testDasSignalPro",
//      gen      = iirGen
//    )
//    new QuartusFlow(iirGen.implH).impl()

//    val firGen = FilterByMag(b, a1, parallel = 1, config)
//    ChainsawSpinalConfig(firGen).generateVerilog(firGen.implH)
//    ChainsawTest(
//      testName = "testDasSignalPro",
//      gen      = firGen
//    )
//    new QuartusFlow(firGen.implH).impl()

    val oldFir = Fir(b, config.coeffType, config.dataType)
    ChainsawTest(
      testName = "testDasSignalPro",
      gen      = oldFir
    )
    new QuartusFlow(oldFir.implH).impl()
  }
}
