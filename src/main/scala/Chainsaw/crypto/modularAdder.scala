package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import Chainsaw.dag._

case class modularAdder(M: BigInt, sub: Boolean = false) extends Dag {
  val k = M.bitLength

  override def name: String = s"modularAdder_${if (sub) "sub" else "add"}_w_${k}"

  /** --------
   * golden model
   * -------- */
  override def impl(dataIn: Seq[Any]): Seq[Any] = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    Seq((if (sub) data.head - data.last else data.head + data.last).mod(M))
  }

  val adder = Cpa(if (sub) BinarySubtractor else BinaryAdder, Seq(k), S2S, withCarry = true).asVertex
  val moder = (if (sub) FineReduction(M, 1, -1) else FineReduction(M, 2, 0)).asVertex

  val a, b = InputVertex(UIntInfo(k))
  val s = OutputVertex(UIntInfo(k))

  adder := (a, b)
  moder := adder.out(0)
  s := moder.out(0)

  graphDone()
}