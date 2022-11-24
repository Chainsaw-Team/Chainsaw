package Chainsaw.dsp

import Chainsaw.ChainsawMetric.{doubleBound, forallBound}
import Chainsaw._
import Chainsaw.dag._
import spinal.core.{Bits, _}
import NumericExt._

import scala.collection.JavaConverters._

case class FilterPrecision(coeffType: NumericType, dataType: NumericType)

case class Scaling(coefficient: Double)(implicit filterPrecision: FilterPrecision)
  extends Combinational {

  import filterPrecision._

  override def comb(dataIn: Seq[Bits]) = {
    val x = dataType.asSFix()
    x.assignFromBits(dataIn.head)
    val ret = (x * coeffType.fromConstant(coefficient)).truncate(dataType.asSFix)
    Seq(ret.asBits)
  }

  override def name = s"scaling_$coefficient".replace('-', 'N').replace('.', '_')

  override def impl(dataIn: Seq[Any]) = null

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(dataType)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl
}

case class Addition()(implicit filterPrecision: FilterPrecision)
  extends Combinational {

  import filterPrecision._

  override def comb(dataIn: Seq[Bits]) = {
    val x, y = dataType.asSFix()
    x.assignFromBits(dataIn.head)
    y.assignFromBits(dataIn.last)
    val ret = x + y
    Seq(ret.asBits)
  }

  override def name = s"addition"

  override def impl(dataIn: Seq[Any]) = null

  override def inputTypes = Seq.fill(2)(dataType)

  override def outputTypes = Seq(dataType)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl
}

case class Var()(implicit filterPrecision: FilterPrecision)
  extends Combinational {

  import filterPrecision._

  override def comb(dataIn: Seq[Bits]) = dataIn

  override def name = s"var"

  override def impl(dataIn: Seq[Any]) = null

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(dataType)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl
}

abstract class FilterGraph(bs: Seq[Double], as: Seq[Double], filterPrecision: FilterPrecision) extends Dag {

  override def impl(dataIn: Seq[Any]) = {
    val b = bs.toArray
    val a = as.toArray
    matlabEngine.feval("filter", b, a, dataIn.asInstanceOf[Seq[Double]].toArray)
      .asInstanceOf[Array[Double]]
  }

  override val metric = ChainsawMetric(frameWise = forallBound(doubleBound(1e-1)))
  override val implMode = Infinite
  implicit val precision: FilterPrecision = filterPrecision

  implicit class FilterUtil(port: DagPort) {
    def *(coeff: Double) = {
      val scaling = Scaling(coeff).asVertex
      scaling := port
      scaling.out(0)
    }

    def +(that: DagPort) = {
      val add = Addition().asVertex
      add := (port, that)
      add.out(0)
    }

    def z(cycle: Int) = {
      val ret = Var().asVertex
      addEdge(port, ret.in(0), cycle)
      ret.out(0)
    }
  }

  override def offset = 1

  override def implH = new ChainsawModule(this) {

    import precision._

    def zero = dataType.fromConstant(0.0)

    val signalMap = vertexSet().asScala.map(vertex => vertex -> dataType.asSFix()).toMap

    inputs.zip(sfixDataIn).foreach { case (v, fix) =>
      signalMap(v) := RegNextWhen(fix, validIn, zero)
    }

    vertexSet().asScala.filterNot(_.isInput)
      .foreach { v =>
        val inputs = v.sources.map { src => signalMap(src).d(getEdge(src, v).weight.toInt, zero) }
        val ret = v.gen.asInstanceOf[Combinational].comb(inputs.map(_.asBits))
        signalMap(v).assignFromBits(ret.head)
        signalMap(v).setName(v.vertexName)
      }

    outputs.zip(sfixDataOut).foreach { case (v, fix) => fix := signalMap(v) }
  }
}

case class SingleFilterGraph(bs: Seq[Double], as: Seq[Double], filterPrecision: FilterPrecision)
  extends FilterGraph(bs, as, filterPrecision) {

  override def name = s"filter_b_${bs.map(_.toInt).mkString("_")}_a_${as.map(_.toInt).mkString("_")}".replace("-", "N")

  import precision._

  val x = InputVertex(dataType)
  val y = OutputVertex(dataType)
  val ret = Var().asVertex

  // transposed direct-2
  val forwards = bs.map(b => x * b).reverse
  val backwards = as.tail.map(a => ret.out(0) * -a).reverse
  val sums = forwards.zip(backwards).map { case (a, b) => a + b }
  ret := (sums :+ forwards.last).reduce { (prev, next) => prev.z(1) + next }
  y := ret.out(0).z(1)

  graphDone()
}

object Unfold {
  def apply(basic: SingleFilterGraph, factor: Int) = {
    new FilterGraph(basic.bs, basic.as, basic.precision) {
      override def name = s"${basic.name}_unfold_$factor"

      import precision._

      val vertexMap: Map[V, Seq[V]] = basic.vertexSet().asScala.map { v =>
        if (v.isInput(basic)) v -> Seq.fill(factor)(InputVertex(dataType).vertex)
        else if (v.isOutput(basic)) v -> Seq.fill(factor)(OutputVertex(dataType).vertex)
        else v -> Seq.fill(factor)(v.gen.asVertex)
      }.toMap

      basic.edgeSet().asScala.foreach { e =>
        val source = e.sourcePort(basic)
        val target = e.targetPort(basic)
        val delay = e.weight(basic)
        (0 until factor).foreach { i =>
          val newSource = vertexMap(source.vertex)(i).out(source.order)
          val newTarget = vertexMap(target.vertex)((i + delay.toInt) % factor).in(target.order)
          val newDelay = (i + delay) / factor
          addEdge(newSource, newTarget, newDelay)
        }
      }
      graphDone()
    }
  }
}