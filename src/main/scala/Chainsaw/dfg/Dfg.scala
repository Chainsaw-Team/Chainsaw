package Chainsaw.dfg

import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph._
import spinal.core._
import spinal.lib._
import spinal.lib.experimental.math.Floating

import scala.collection.JavaConverters._
import scala.collection.mutable

class Dfg extends DirectedWeightedPseudograph[DfgVertex, DfgEdge](classOf[DfgEdge]) with Area {

  implicit val background: Dfg = this

  // attributes
  var useFloating = false
  var useStream   = false

  def vertexSeq: Seq[DfgVertex] = vertexSet().asScala.toList
  def edgeSeq: Seq[DfgEdge]     = edgeSet().asScala.toList

  // linked hash map keep keys in insertion order
  val floatingSignalMap = mutable.LinkedHashMap[Io, Stream[Floating]]()
  val fixedSignalMap    = mutable.LinkedHashMap[Io, Stream[AFix]]()

  def floatingInputs  = floatingSignalMap.filter(_._1.isInstanceOf[Input])
  def floatingOutputs = floatingSignalMap.filter(_._1.isInstanceOf[Output])
  def fixedInputs     = fixedSignalMap.filter(_._1.isInstanceOf[Input])
  def fixedOutputs    = fixedSignalMap.filter(_._1.isInstanceOf[Output])

  def isRecursive: Boolean = new CycleDetector(this).detectCycles()

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
