//package Chainsaw.dsp
//
//import org.jgrapht._
//import org.jgrapht.graph._
//import scala.collection.JavaConverters._
//
//import Chainsaw._
//import Chainsaw.xilinx._
//
//import spinal.core._
//import spinal.lib._
//import spinal.core.sim._
//import spinal.sim._
//import spinal.lib.fsm._
//import spinal.lib.bus._
//
//trait DfgNode {
//  def executionTime: Double
//  def delay: Int
//
//  def impl: ChainsawVec => ChainsawVec
//
//  def slow(factor: Int): Seq[DfgNode]
//}
//
//case class Mult() extends DfgNode {
//  override def executionTime: Double = 2.0
//
//  override def delay: Int = 2
//
//  override def impl: ChainsawVec => ChainsawVec = vec => Vec(vec.reduce(_ * _).truncated)
//
//  override def slow(factor: Int): Seq[DfgNode] = this +: Seq.fill(factor - 1)(Mult())
//}
//
//case class Add() extends DfgNode {
//  override def executionTime: Double = 1.0
//
//  override def delay: Int = 1
//
//  override def impl: ChainsawVec => ChainsawVec = vec => Vec(vec.reduce(_ + _).truncated)
//
//  override def slow(factor: Int): Seq[DfgNode] = this +: Seq.fill(factor - 1)(Add())
//}
//
//case class DelayVar()
//
//case class DelayExpr(constPart: Int, varPart: String) {
//  def +(that: DelayExpr): DelayExpr = this.constPart + that.constPart
//  def /(that: DelayExpr): DelayExpr
//}
//
//case class ConstantDelay(delay: Int) extends DelayExpr {
//
//  assert(delay >= 0)
//  override def +(that: DelayExpr): DelayExpr = ConstantDelay(delay + that)
//
//  override def /(that: DelayExpr): DelayExpr = ConstantDelay(delay / that) // floor(this/that)
//}
//
//trait DfgEdge {
//
//  def delay: DelayExpr
//  def impl: ChainsawVec => ChainsawVec
//
//  def portOut: Int
//
//  def portIn: Int
//
//}
//
//case class ConstantEdge(delay: ConstantDelay, portOut: Int, portIn: Int) extends DfgEdge {
//
//  override def impl: ChainsawVec => ChainsawVec = _.map(_.d(delay.delay))
//}
//
//class Dfg extends WeightedPseudograph[DfgNode, DfgEdge](classOf[DfgEdge]) {
//
//  def unfold(factor: Int): Unit = {
//    val orderedNodes = vertexSet().asScala.toSeq
//
//    val nodeMap: Map[DfgNode, Seq[DfgNode]] = orderedNodes.map { node => // node -> its duplicates
//      val duplicates = node.slow(factor)
//      duplicates.foreach(this.addVertex)
//      node -> duplicates
//    }.toMap
//
//    val edges = edgeSet().asScala.toSeq // keep it
//    edges.foreach { edge =>
//      val srcDuplicates    = nodeMap(this.getEdgeSource(edge))
//      val targetDuplicates = nodeMap(this.getEdgeTarget(edge))
//      (0 until factor).foreach { i =>
//        val newEdge = ConstantEdge() // TODO: implement variable delay
//      }
//    }
//
//  }
//
//}
//
//object Dfg {}
