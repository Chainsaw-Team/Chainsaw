package Chainsaw.dfg

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._
import spinal.lib.experimental.math.Floating

import scala.collection.mutable
import Chainsaw._
object DfgBuildFloating {

  def apply(dfg: Dfg): Unit = {

    implicit val background = dfg

    // attributes tobe used, they provide the "environment" of "synthesis"
    val vertexSeq: Seq[DfgVertex] = background.vertexSeq
    val inputs                    = vertexSeq.filter(_.isInstanceOf[Input]).map(_.asInstanceOf[Input])
    val outputs                   = vertexSeq.filter(_.isInstanceOf[Output]).map(_.asInstanceOf[Output])
    val constants                 = vertexSeq.filter(_.isInstanceOf[Constant]).map(_.asInstanceOf[Constant])

    val signalMap: mutable.Map[DfgVertex, Seq[Bits]] =
      mutable.Map[DfgVertex, Seq[Bits]]() // storing the output signals of implemented nodes
    background.vertexSeq.foreach(v => signalMap(v) = Seq.fill(v.outCount)(Bits(32 bits)))

    def implVertex(target: DfgVertex): Unit = {
      // step1: constructing all driving signals(dataIns)
      val inputs: Seq[Bits] = target.incomingEdges
        .sortBy(_.inId)
        .map(e => signalMap(e.source)(e.outId).d(e.delay, init = Bits(32 bits).getZero))
      // step2: driving the operator and get the output
      signalMap(target).zip(target.impl(inputs)).foreach { case (placeholder, ret) => placeholder := ret }
    }

    val dataIn  = inputs.map(_.floating)
    val dataOut = outputs.map(_.floating)

    constants.foreach(constant =>
      signalMap(constant)(0).assignFromBits(B(java.lang.Float.floatToIntBits(constant.value), 32 bits))
    )
    vertexSeq.foreach(implVertex)
    outputs.zip(dataOut).foreach { case (vertex, out) => out.assignFromBits(signalMap(vertex)(0)) }
    inputs.zip(dataIn).foreach { case (vertex, in) => signalMap(vertex)(0) := in.asBits }
  }

}
