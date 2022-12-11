package Chainsaw.deprecated

import Chainsaw.crypto.EllipticCurve

case class EllipticCurveAdder(ecCurve: EllipticCurve, width: Int, constantModulus: Option[BigInt])
  extends Dag {

  val X1, Y1, Z1, T1, X2, Y2, Z2, T2 = InputVertex(UIntInfo(width))
  val X, Y, Z, T = OutputVertex(UIntInfo(width))

  X1.vertex.setName("X1")
  Y1.vertex.setName("Y1")
  Z1.vertex.setName("Z1")
  T1.vertex.setName("T1")
  X2.vertex.setName("X2")
  Y2.vertex.setName("Y2")
  Z2.vertex.setName("Z2")
  T2.vertex.setName("T2")
  X.vertex.setName("X")
  Y.vertex.setName("Y")
  Z.vertex.setName("Z")
  T.vertex.setName("T")

  val k = BigInt("0196bab03169a4f2ca0b7670ae65fc7437786998c1a32d217f165b2fe0b32139735d947870e3d3e4e02c125684d6e016", 16) //

  val r0Mul = FakeMult(width).asVertex
  val r1CMul = FakeCMult(width, k).asVertex

  val R1Sub = FakeAdd(width, sub = true).asVertex
  val R2Sub = FakeAdd(width, sub = true).asVertex
  val R3Add = FakeAdd(width, sub = false).asVertex
  val R4Add = FakeAdd(width, sub = false).asVertex

  val R5Mul = FakeMult(width).asVertex
  val R6Mul = FakeMult(width).asVertex
  val R7Mul = FakeMult(width).asVertex
  val R8CMul = FakeCMult(width, BigInt(2)).asVertex

  val R9Sub = FakeAdd(width, sub = true).asVertex
  val R10Sub = FakeAdd(width, sub = true).asVertex
  val R11Add = FakeAdd(width, sub = false).asVertex
  val R12Add = FakeAdd(width, sub = false).asVertex

  val XMul = FakeMult(width).asVertex
  val YMul = FakeMult(width).asVertex
  val ZMul = FakeMult(width).asVertex
  val TMul = FakeMult(width).asVertex

  r0Mul := (Z1, Z2)
  r1CMul := T2

  R1Sub := (Y1, X1)
  R2Sub := (Y2, X2)
  R3Add := (Y1, X1)
  R4Add := (Y2, X2)

  R5Mul := (R1Sub.out(0), R2Sub.out(0))
  R6Mul := (R3Add.out(0), R4Add.out(0))
  R7Mul := (T1, r1CMul.out(0))
  R8CMul := r0Mul.out(0)

  R9Sub := (R6Mul.out(0), R5Mul.out(0))
  R10Sub := (R8CMul.out(0), R7Mul.out(0))
  R11Add := (R8CMul.out(0), R7Mul.out(0))
  R12Add := (R6Mul.out(0), R5Mul.out(0))

  XMul := (R9Sub.out(0), R10Sub.out(0))
  YMul := (R11Add.out(0), R12Add.out(0))
  ZMul := (R10Sub.out(0), R11Add.out(0))
  TMul := (R9Sub.out(0), R12Add.out(0))

  X := XMul.out(0)
  Y := YMul.out(0)
  Z := ZMul.out(0)
  T := TMul.out(0)

  this.exportPng("EcAddExample")

  graphDone()

  override def name: String = "padd"

  /** --------
   * golden model
   * -------- */
  override def impl(dataIn: Seq[Any]): Seq[Any] = null
}
