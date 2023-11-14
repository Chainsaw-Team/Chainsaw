package Chainsaw.dfg

import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph._
import spinal.core._
import spinal.lib.experimental.math.Floating

import java.io.FileOutputStream
import scala.collection.JavaConverters._
import spinal.core._
import spinal.core.sim._
import Chainsaw.arithmetic.floating._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class Dfg extends DirectedWeightedPseudograph[DfgVertex, DfgEdge](classOf[DfgEdge]) with Area {

  implicit val background: Dfg = this
  var useFloating              = false
  var useStream                = false

  def vertexSeq: Seq[DfgVertex] = vertexSet().asScala.toList
  def edgeSeq: Seq[DfgEdge]     = edgeSet().asScala.toList

  val floatingInputs  = mutable.Map[Input, Floating]()
  val floatingOutputs = mutable.Map[Output, Floating]()
  val fixedInputs     = mutable.Map[Input, AFix]()
  val fixedOutputs    = mutable.Map[Output, AFix]()

  def isRecursive: Boolean = new CycleDetector(this).detectCycles()

  private def implFloating: DfgImplFloating = DfgImplFloating(this)

  /** do simulation using single-precision floating-point number
    * @param stimulus
    * @return
    */
  def runFloating(stimulus: Seq[Seq[Float]]): mutable.Seq[Seq[Float]] = {
    val ret = ArrayBuffer[Seq[Float]]()
    SimConfig.withFstWave.compile(implFloating).doSim { dut =>
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

  def exportDrawIo(name: String): Unit = DfgToDrawIo(this, name)

  def clarify() = {
    // rule0: remove redundant SISO NoOps
    def isRedundant(vertex: DfgVertex) = vertex.isInstanceOf[Intermediate] && vertex.outgoingEdges.length == 1
    val candidates                     = vertexSeq.filter(isRedundant)
    candidates.foreach { v =>
      val edgeIn  = v.incomingEdges.head
      val edgeOut = v.outgoingEdges.head
      addEdge(edgeIn.source, edgeOut.target, new DfgEdge(edgeIn.delay + edgeOut.delay, edgeIn.outId, edgeOut.inId))
      removeVertex(v)
    }
  }

  def build() = {

    DfgBuildFloating(this)

  }
}
