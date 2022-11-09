package Chainsaw

import spinal.core._

import scala.language.postfixOps
import scala.util.Random
import dag._
import xilinx._
import arithmetic._

// a simple ChainsawGenerator
case class ChainsawAddGen(width: Int) extends ChainsawGenerator {

  override def name = s"adder_$width"

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)

  override var inputTypes = Seq.fill(2)(UIntInfo(width))
  override var outputTypes = Seq(UIntInfo(width + 1))
  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = 1

  override def implH: ChainsawModule = new ChainsawModule(this) {
    dataOut.head := dataIn.map(_.asUInt).reduce(_ +^ _).d().asBits
  }

  utilEstimation = VivadoUtilRequirement(lut = width + 2, carry8 = width.divideAndCeil(8))
  fmaxEstimation = 600 MHz
}

// a simple Dag
case class AdderGraph(width: Int) extends Dag {
  override def name = s"adderGraph_$width"

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)

  // the generator can be instantiated outside the SpinalHDL context
  val generator = ChainsawAddGen(width)
  //  val generatorAnother = ChainsawAddGen(width) // warning: generator already exist, as it is the same as the previous generator
  val generatorPlus1 = ChainsawAddGen(width + 1)

  // declare components
  // declare IO
  val i0, i1, i2, i3 = InputVertex(UIntInfo(width))
  val o = OutputVertex(UIntInfo(width + 2))
  // declare submodules
  val v0, v1 = generator.asVertex
  val v2 = generatorPlus1.asVertex

  v0 := (i0, i1)
  v1 := (i2, i3)
  v2 := (v0.out(0), v1.out(0))
  o := v2.out(0)

  graphDone() // this must be invoked explicitly
}

object NestedAdderGraph extends Dag {
  override def name = s"nestedAdderGraph"

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)

  // the generator can be instantiated outside the SpinalHDL context
  val addGraph0 = AdderGraph(10)
  val addGraph1 = AdderGraph(12)
  // declare components
  // declare IO
  val is = Seq.fill(16)(InputVertex(UIntInfo(10)))
  val o = OutputVertex(UIntInfo(14))
  // declare submodules
  val level1 = Seq.fill(4)(addGraph0.asVertex)
  val level2 = addGraph1.asVertex

  level1.zip(is.grouped(4).toSeq).foreach { case (v, ports) => v := (ports: _*) }
  val level1Port = level1.map(_.out(0))
  level2 := (level1Port: _*)
  o := level2.out(0)

  graphDone() // this must be invoked explicitly
}

case class CpaGraph() extends Dag {
  override val name = "cpaGraph"

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)

  // declare components
  // declare IO
  val i = InputVertex(UIntInfo(40))
  val o = OutputVertex(UIntInfo(40))
  // declare submodules
  val widths = Seq.fill(4)(10)
  val add0 = Cpa(BinaryAdder, widths, S2M).asVertex // aligned -> diff
  val add1 = Cpa(BinaryAdder, widths, M2M).asVertex // diff -> diff
  val add2 = Cpa(BinaryAdder, widths, M2S).asVertex // diff -> aligned

  add0.in(0) := i
  add1.<<(add0)
  add2.<<(add1)
  o := add2.out(0)

  graphDone() // this must be invoked explicitly
}

object UseGenerator {
  def apply() = {
    verbose = 1

    val add10 = ChainsawAddGen(10) // declare a generator

    // behavioral test
    val test = ChainsawTest("testAdd10", add10, Seq.fill(10)(BigInt(10, Random)))
    test.doTest()

    // performance tests
    ChainsawSynth(add10, name = "synthAdd10", withRequirement = true) // by synth
  }
}


object UseDag {
  def apply(): Unit = {
    val adderGraph = AdderGraph(10)

    ChainsawTest("adderGraph", adderGraph, Seq.fill(40)(BigInt(10, Random))).doTest()
    adderGraph.exportPng()

    val nestedGraph = NestedAdderGraph

    ChainsawTest("nestedGraph", nestedGraph, Seq.fill(64)(BigInt(10, Random))).doTest()
    nestedGraph.exportPng("nested")

    nestedGraph.flatten()
    ChainsawTest("nestedGraph", nestedGraph, Seq.fill(64)(BigInt(10, Random))).doTest()
    nestedGraph.exportPng("flattened")
  }
}

object SelfTest extends App {
  //  UseGenerator()
  UseDag()
}
