package Chainsaw.dfg

import Chainsaw.NumericType
import FloatingOperators._
import FixOperators._
import spinal.core._
import spinal.core.internals.PhaseContext
import spinal.lib.experimental.math.Floating
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._
import Chainsaw.arithmetic.floating._

import scala.collection.mutable.ArrayBuffer

case class SystolicDfg() extends Dfg {
  val coeffs: Seq[Float] = Seq.fill(6)(1.0.toFloat)
  val i: Signal          = SIn()
  val o: Signal          = SOut()

  private val delayLine   = Seq.iterate(i, coeffs.length)(_.d(2))
  val scaled: Seq[Signal] = delayLine.zip(coeffs).map { case (signal, coeff) => signal * coeff }
  o := scaled.reduce((a: Signal, b: Signal) => (a + b).d())
}

object SystolicDfg extends App {

  SpinalConfig().generateVerilog {
    new Component {
      val systolicGraph = SystolicDfg()
      val dataIn        = in(systolicGraph.floatingInputs.values.head)
      val dataOut       = out(systolicGraph.floatingOutputs.values.head)
      systolicGraph.build()
      systolicGraph.exportDrawIo("systolicGraph")
    }
  }

  val ret = DfgTest(SystolicDfg(), Seq.fill(20)(Seq(1.0f)))
  ret.foreach(vec => println(vec.head))
}

object DfgWrapper {

  def apply(dfg: => Dfg) = {
    new Component {
      val graph   = dfg
      val dataIn  = in(Vec(graph.floatingInputs.values)) // TODO: order?
      val dataOut = out(Vec(graph.floatingOutputs.values))
      graph.build()

    }
  }
}

object DfgTest {
  def apply(dfg: => Dfg, stimulus: Seq[Seq[Float]]) = {
    val ret = ArrayBuffer[Seq[Float]]()
    SimConfig.withFstWave.compile(DfgWrapper(dfg)).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}
      dataIn.foreach { port => port #= 0.0 }
      clockDomain.forkStimulus(2)

      stimulus.map { inputs =>
        inputs.zip(dataIn).foreach { case (input, port) => port #= input }
        clockDomain.waitSampling()
        ret += dataOut.map(_.toFloat)
      }
    }
    ret

  }
}
