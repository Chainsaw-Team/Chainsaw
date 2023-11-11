package Chainsaw.dfg

import Chainsaw._
import spinal.lib.experimental.math.Floating
//import jdk.jfr.Experimental
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

case class DfgImplFloating(dfg: Dfg) extends Module {

  // TODO: do regs merging before implementation
  implicit val preprocessed = dfg

  // attributes tobe used, they provide the "environment" of "synthesis"
  val vertexSeq: Seq[DfgVertex] = preprocessed.vertexSeq
  val inputs                    = vertexSeq.filter(_.isInstanceOf[Input]).map(_.asInstanceOf[Input])
  val outputs                   = vertexSeq.filter(_.isInstanceOf[Output]).map(_.asInstanceOf[Output])
  val constants                 = vertexSeq.filter(_.isInstanceOf[Constant]).map(_.asInstanceOf[Constant])

  def sp() = Floating(8, 23)

//  val dataIn  = in(Vec(inputs.map(_ => sp())))
//  val dataOut = out(Vec(outputs.map(_ => sp())))
  val dataIn  = in(Vec(inputs.map(_.floating)))
  val dataOut = out(Vec(outputs.map(_ .floating)))

  val signalMap: mutable.Map[DfgVertex, Seq[Bits]] =
    mutable.Map[DfgVertex, Seq[Bits]]() // storing the output signals of implemented nodes
  preprocessed.vertexSeq.foreach(v => signalMap(v) = Seq.fill(v.outCount)(Bits(32 bits)))

  def implVertex(target: DfgVertex): Unit = {
    // step1: constructing all driving signals(dataIns)
    val inputs: Seq[Bits] = target.incomingEdges
      .sortBy(_.inId)
      .map(e => signalMap(e.source)(e.outId).d(e.delay, init = Bits(32 bits).getZero))
    // step2: driving the operator and get the output
    signalMap(target).zip(target.impl(inputs)).foreach { case (placeholder, ret) => placeholder := ret }
  }


  constants.foreach(constant =>
    signalMap(constant)(0).assignFromBits(B(java.lang.Float.floatToIntBits(constant.value), 32 bits))
  )
  vertexSeq.foreach(implVertex)
  outputs.zip(dataOut).foreach { case (vertex, out) => out.assignFromBits(signalMap(vertex)(0)) }

}
