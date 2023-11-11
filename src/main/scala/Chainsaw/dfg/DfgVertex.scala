package Chainsaw.dfg

import Chainsaw.NumericType
import spinal.core.{AFix, Bits}
import spinal.lib.experimental.math.Floating

import scala.collection.JavaConverters._

abstract class DfgVertex(implicit dfg: Dfg) {

  val name: String

  // hardware information
  def inputTypes: Seq[NumericType]
  def outputTypes: Seq[NumericType]
  def delay: Int
  def executionTime: Double

  override def toString: String = name // TODO: get name by reflection

  def impl(inputs: Seq[Bits]): Seq[Bits]

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

  override def impl(inputs: Seq[Bits]): Seq[Bits] = inputs
}

abstract class Io(dataType: NumericType)(implicit dfg: Dfg) extends NoOp(dataType) {
  def floating: Floating
  def fixed: AFix

}

class Input(dataType: NumericType)(implicit dfg: Dfg) extends Io(dataType) {
  override val name: String = "I"

  dfg.floatingInputs += this    -> Floating(8, 23)
  dfg.fixedInputs += this -> dataType()

  override def floating: Floating = dfg.floatingInputs(this)

  override def fixed: AFix = dfg.fixedInputs(this)
}

class Output(dataType: NumericType)(implicit dfg: Dfg) extends Io(dataType) {
  override val name: String = "O"

  dfg.floatingOutputs += this    -> Floating(8, 23)
  dfg.fixedOutputs += this -> dataType()

  override def floating: Floating = dfg.floatingOutputs(this)

  override def fixed: AFix = dfg.fixedOutputs(this)
}

class Constant(val value: Float, dataType: NumericType)(implicit dfg: Dfg) extends NoOp(dataType) {
  override val name: String = value.toString
}

class Intermediate(dataType: NumericType)(implicit dfg: Dfg) extends NoOp(dataType)
