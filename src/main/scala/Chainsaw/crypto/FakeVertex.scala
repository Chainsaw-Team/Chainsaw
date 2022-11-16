package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import Chainsaw.dag._

case class FakeMult(width: Int) extends Dag {
  override def name = "mult"

  override def impl(dataIn: Seq[Any]) = null

  val x, y = InputVertex(UIntInfo(width))
  val cpa = Cpa(BinarySubtractor, Seq(width), S2S, withCarry = false).asVertex
  val z = OutputVertex(UIntInfo(width))
  cpa := (x, y)
  z := cpa.out(0)

  graphDone()
}

case class FakeAdd(width: Int, sub: Boolean) extends Dag {
  override def name = if (sub) "sub" else "add"

  override def impl(dataIn: Seq[Any]) = null

  val x, y = InputVertex(UIntInfo(width))
  val cpa = Cpa(BinarySubtractor, Seq(width), S2S, withCarry = false).asVertex
  val z = OutputVertex(UIntInfo(width))
  cpa := (x, y)
  z := cpa.out(0)

  graphDone()
}

case class FakeCMult(width: Int, constant: BigInt) extends Dag {
  override def name = s"cmult_${constant.hashCode()}"

  override def impl(dataIn: Seq[Any]) = null

  val x = InputVertex(UIntInfo(width))
  val z = OutputVertex(UIntInfo(width))
  z := x

  graphDone()
}