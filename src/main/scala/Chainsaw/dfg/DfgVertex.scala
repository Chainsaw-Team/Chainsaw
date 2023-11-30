package Chainsaw.dfg

import Chainsaw.NumericType
import spinal.core.{AFix, Bits}
import spinal.lib.experimental.math.Floating

import scala.collection.JavaConverters._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

abstract class DfgVertex(implicit dfg: Dfg) {

  val name: String

  // hardware information
  def inputTypes: Seq[NumericType]
  def outputTypes: Seq[NumericType]
  def delay: Int
  def executionTime: Double

  override def toString: String = name // TODO: get name by reflection

  def implFloating(inputs: Seq[Floating]): Seq[Floating] = ???
  def implFixed(inputs: Seq[AFix]): Seq[AFix] = ???

  def inCount = inputTypes.length

  def outCount = outputTypes.length

  // graph methods

  def incomingEdges: Seq[DfgEdge] = dfg.incomingEdgesOf(this).asScala.toSeq
  def outgoingEdges: Seq[DfgEdge] = dfg.outgoingEdgesOf(this).asScala.toSeq

  dfg.addVertex(this)
}

abstract class Operator(implicit dfg: Dfg) extends DfgVertex

class NoOp(dataType: NumericType)(implicit dfg: Dfg) extends DfgVertex {
  override val name: String = "NoOp"

  override def inputTypes: Seq[NumericType] = Seq(dataType)

  override def outputTypes: Seq[NumericType] = Seq(dataType)

  override val delay: Int            = 0
  override val executionTime: Double = 0.0

  override def implFloating(inputs: Seq[Floating]): Seq[Floating] = inputs
}

abstract class Io(dataType: NumericType)(implicit dfg: Dfg) extends NoOp(dataType) {

  def floatingStream: Stream[Floating] = dfg.floatingSignalMap(this)
  def floatingFlow: Flow[Floating] = floatingStream.asFlow
  def floating: Floating = floatingStream.payload

  def fixedStream: Stream[AFix] = dfg.fixedSignalMap(this)

  def fixedFlow: Flow[AFix] = fixedStream.asFlow
  def fixed: AFix = fixedStream.payload

  dfg.floatingSignalMap += this -> Stream(Floating(8, 23)).setName(this.name)
  dfg.fixedSignalMap += this -> Stream(dataType()).setName(this.name)

}

class Input(dataType: NumericType)(implicit dfg: Dfg) extends Io(dataType) {
  override val name: String = "I"
}

class Output(dataType: NumericType)(implicit dfg: Dfg) extends Io(dataType) {
  override val name: String = "O"
}

class Constant(val value: Float, dataType: NumericType)(implicit dfg: Dfg) extends NoOp(dataType) {
  override val name: String = value.toString
}

class Intermediate(dataType: NumericType)(implicit dfg: Dfg) extends NoOp(dataType)
