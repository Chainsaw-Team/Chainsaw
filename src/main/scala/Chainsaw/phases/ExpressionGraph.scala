package Chainsaw.phases

import Chainsaw.{ChainsawBaseModule, FixedLatency, logger, verbose}
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import optimus.optimization.{MPModel, add, minimize, release, start}
import org.jgrapht._
import org.jgrapht.graph._
import spinal.core._
import spinal.core.internals._
import Chainsaw._
import Chainsaw.xilinx._

import scala.collection.JavaConverters._
import scala.collection.mutable

class SpinalEdge(val graph: ExpressionGraph) extends DefaultWeightedEdge {

  override def toString: String = s"${graph.getEdgeWeight(this)}"
}

/** graph of SpinalHDL internals
 *
 */
class ExpressionGraph extends
  DirectedWeightedPseudograph[ScalaLocated, SpinalEdge](classOf[SpinalEdge]) {

  val inputs = mutable.HashSet[ScalaLocated]()
  val outputs = mutable.HashSet[ScalaLocated]()

  def addEdge(src: ScalaLocated, des: ScalaLocated, weight: Double): Unit = {
    val e = new SpinalEdge(this)
    addEdge(src, des, e)
    setEdgeWeight(e, weight)
  }

  def addInput(v: ScalaLocated) = {
    addVertex(v)
    inputs += v
  }

  def addOutput(v: ScalaLocated) = {
    addVertex(v)
    outputs += v
  }

  def exportPng(name: String) = ExportGraph(this, name)

  def printConnectivity(): Unit = {
    val sets = new alg.connectivity.ConnectivityInspector(this).connectedSets().asScala
    logger.info(s"connectivity size:\n\t ${sets.map(_.size()).mkString(" ")}")
    sets.sortBy(_.size()).init.foreach(isolated => logger.info(s"isolated set:\n\t ${isolated.asScala.mkString("\n\t")}"))
  }

}