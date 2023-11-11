package Chainsaw.dfg

import Chainsaw.NumericType
import FloatingOperators._
import FixOperators._
import spinal.core._
import spinal.core.internals.PhaseContext
import spinal.lib.experimental.math.Floating

object Demo0 extends App {

  val accGraph = new Dfg {
    val i   = SIn()
    val acc = SOut()
    acc := (i + acc.d(1)) // caution, := will take effect before +
  }

  // visualization
  accGraph.exportDrawIo("accGraph")
  // simulation
  val stimulus = (0 until 10).map(i => Seq(i.toFloat))
  accGraph.runFloating(stimulus).foreach(println)
}

object Demo1 extends App {

  val adderGraph = new Dfg {
    val is  = Seq.fill(4)(SIn())
    val o   = SOut()
    val im0 = (is(0) + is(1)).d()
    val im1 = (is(2) + is(3)).d()
    o := (im0 + im1).d()
  }

  adderGraph.exportDrawIo("adderGraph")

  val stimulus = (0 until 10).map(i => Seq.fill(4)(i.toFloat))
  adderGraph.runFloating(stimulus).foreach(println)
}

case class SystolicDfg() extends Dfg {
  val coeffs = Seq.fill(6)(1.0.toFloat)
  val i      = SIn()
  val o      = SOut()

  val delayLine = Seq.iterate(i, coeffs.length)(_.d(2))
  val scaled    = delayLine.zip(coeffs).map { case (signal, coeff) => signal * coeff }
  o := scaled.reduce((a: Signal, b: Signal) => (a + b).d())
}

object SystolicDfg extends App {

  def inVirtualGlob[T](func: => T): T = {

    val virtualGlob = new GlobalData(SpinalConfig())
    virtualGlob.phaseContext = new PhaseContext(SpinalConfig())
    GlobalData.set(virtualGlob)
    val ret = func
    ret
  }

  inVirtualGlob {
    val dfg = SystolicDfg()
    dfg.exportDrawIo("systolicGraph")
//    val stimulus = (0 until 20).map(i => Seq(1.0f))
//    dfg.runFloating(stimulus).foreach(println)

  }
}

object Demo2Component extends App { // systolic FIR

  SpinalConfig().generateVerilog {
    new Component {

      val coeffs = Seq.fill(6)(1.0.toFloat)

      val systolicGraph = new Dfg {
        val i = SIn()
        val o = SOut()

        val delayLine = Seq.iterate(i, coeffs.length)(_.d(2))
        val scaled    = delayLine.zip(coeffs).map { case (signal, coeff) => signal * coeff }
        o := scaled.reduce((a: Signal, b: Signal) => (a + b).d())
      }

      val dataIn  = in(Floating(8, 23))
      val dataOut = out(Floating(8, 23))

      systolicGraph.i.floating := dataIn
      dataOut                  := systolicGraph.o.floating

    }
  }
}

object Demo3 extends App { // sorting network

  val bitonicGraph = new Dfg {
    val sel    = SIn(NumericType.Bool())
    val i0, i1 = SIn(NumericType.SFix(3, 15))

    val o0, o1   = SOut()
    val (t0, t1) = Switch(sel, i0, i1)
    o0 := t0.d()
    o1 := t1.d(1)
  }

  val stimulus = (0 until 20).map(i => Seq(0.0f, 1.0f, 2.0f))
  bitonicGraph.runFloating(stimulus).foreach(println)

}

object Demo4 extends App { // adder graph

}
