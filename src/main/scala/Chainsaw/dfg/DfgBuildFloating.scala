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

//    dfg.clarify()

    implicit val background: Dfg = dfg
    // attributes tobe used, they provide the "environment" of "synthesis"
    val vertexSeq: Seq[DfgVertex] = background.vertexSeq
    val inputs = vertexSeq.filter(_.isInstanceOf[Input]).map(_.asInstanceOf[Input])
    val outputs = vertexSeq.filter(_.isInstanceOf[Output]).map(_.asInstanceOf[Output])
    val constants = vertexSeq.filter(_.isInstanceOf[Constant]).map(_.asInstanceOf[Constant])

    if (!dfg.useStream) { // raw implementation // TODO: implement this as a subset of Stream implementation
      // storing the output signals of implemented nodes
      val signalMap: mutable.Map[DfgVertex, Seq[Floating]] = mutable.Map[DfgVertex, Seq[Floating]]()
      // initialize input, output,constant and placeholders
      background.vertexSeq.foreach(v => signalMap(v) = Seq.fill(v.outCount)(Floating(8, 23)))
      inputs.foreach(i => signalMap(i)(0) := i.floating )
      outputs.foreach(o => o.floating := (signalMap(o)(0)))
      constants.foreach(c => signalMap(c)(0).assignFromBits(B(java.lang.Float.floatToIntBits(c.value), 32 bits)))

      // implement vertices one by one
      def implVertex(target: DfgVertex): Unit = {
        // step1: constructing all driving signals(dataIns)
        val inputs: Seq[Floating] = target.incomingEdges
          .sortBy(_.inId)
          .map(e => signalMap(e.source)(e.outId).d(e.delay, init = Floating(8, 23).getZero))
        // step2: driving the operator and get the output
        signalMap(target).zip(target.implFloating(inputs)).foreach { case (placeholder, ret) => placeholder := ret }
      }
      vertexSeq.foreach(implVertex)
    }
    else {
      val signalMap: mutable.Map[DfgVertex, Seq[Stream[Floating]]] = mutable.Map[DfgVertex, Seq[Stream[Floating]]]()
      // initialize input, output,constant and placeholders
      background.vertexSeq.foreach(v => signalMap(v) = Seq.fill(v.outCount)(Stream(Floating(8, 23))))
      inputs.foreach(i =>  i.floatingStream >>  signalMap(i)(0))
      outputs.foreach(o => signalMap(o)(0) >> o.floatingStream)
      constants.foreach { c =>
        val stream = signalMap(c)(0)
        stream.valid := True
        stream.payload.assignFromBits(B(java.lang.Float.floatToIntBits(c.value), 32 bits))
      }

      // implement vertices one by one
      // TODO: pay attention to s2mPipe
      def implVertex(target: DfgVertex): Unit = {
        println(s"implementing ${target.name}")
        val inputs: Seq[Stream[Floating]] = target.incomingEdges
          .sortBy(_.inId)
          .map(e => (signalMap(e.source)(e.outId), e.delay))
          .map { case (stream, d) => Seq.iterate(stream, d + 1)(_.m2sPipe()).last }
        // step2: datapath
        val rets: Seq[Floating] = target.implFloating(inputs.map(_.payload)) // TODO: pay attention when dfgVertex has inner delay
        // step3: sync by StreamJoin
        if (inputs.nonEmpty) {
          inputs.foreach(_.ready.allowOverride) // inputs may contain repeated signals
         val retStream = Seq.iterate(StreamJoin(inputs), target.delay + 1)(_.m2sPipe()).last
          signalMap(target).zip(rets).foreach { case (placeholder, ret) =>
            placeholder.payload := ret
            placeholder.valid := retStream.valid
            retStream.ready := placeholder.ready
          }
        }

      }
      // TODO: sync by StreamFork before implementation
      vertexSeq.foreach(implVertex)
    }
  }
}
