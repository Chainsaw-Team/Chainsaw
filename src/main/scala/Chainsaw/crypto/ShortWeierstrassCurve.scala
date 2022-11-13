package Chainsaw.crypto

import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

case class ShortWeierstrassCurve(override val modulus: IntZ, override val c: IntZ, override val d: IntZ)
  extends EllipticCurve(modulus, 0, 0, c, d) {

  implicit val ecc: ShortWeierstrassCurve = this

  /** algorithm for padd & pdbl on homogeneous projective coordinate, 12M required
   *
   * @see ''Complete addition formulas for prime order elliptic curves, Joost Renes, Craig Costello, Lejla Batina'' for the algorithm
   * @see ''PipeMSM: Hardware Acceleration for Multi-Scalar Multiplication, Charles. F. Xavier'' [[https://eprint.iacr.org/2022/999.pdf]] for the pipeline
   */
  def paddHomo(p0: EcPointProj, p1: EcPointProj) = {
    val zp = Zp(modulus)
    val (x1, x2, y1, y2, z1, z2) = (p0.x, p1.x, p0.y, p1.y, p0.z, p1.z)
    // parallel version with a well-designed pipeline
    // stage0
    val m00 = zp.multiply(x1, x2) // x1x2
    val m01 = zp.multiply(y1, y2) // y1y2
    val m02 = zp.multiply(z1, z2) // z1z2
    val a00 = zp.add(x1, y1) // x1+y1
    val a01 = zp.add(x2, y2) // x2+y2
    val a02 = zp.add(x1, z1) // x1+z1
    val a03 = zp.add(x2, z2) // x2+z2
    val a04 = zp.add(y1, z1) // y1+z1
    val a05 = zp.add(y2, z2) // y2+z2
    // stage1
    val a10 = zp.add(m00, m01) // x1x2 + y1y2
    val a11 = zp.add(m00, m02) // x1x2 + z1z2
    val a12 = zp.add(m01, m02) // y1y2 + z1z2
    val m10 = zp.multiply(a00, a01) // x1x2 + y1y2 + x1y2 + x2y1
    val m11 = zp.multiply(a02, a03) // x1x2 + z1z2 + x1z2 + x2z1
    val m12 = zp.multiply(a04, a05) // y1y2 + z1z2 + y1z2 + y2z1
    // stage 2
    val tri20 = m02 * 3 // 3z1z2
    val s20 = zp.subtract(m10, a10) // x1y2 + x2y1
    val s21 = zp.subtract(m11, a11) // x1z2 + x2z1
    val s22 = zp.subtract(m12, a12) // y1z2 + y2z1
    // stage 3
    val tri30 = m00 * 3 // 3x1x2
    val a30 = zp.add(m01, tri20) // y1y2 + 3z1z2
    val s30 = zp.subtract(m01, tri20) // y1y2 - 3z1z2
    val tri31 = s21 * 3 // 3(x1z2 + x2z1)
    // stage 4
    val m40 = zp.multiply(s30, s20) // (y1y2 - 3z1z2)(x1y2 + y2x1)
    val m41 = zp.multiply(tri31, s22) // 3(x1z2 + x2z1)(y1z2 + y2z1)
    val m42 = zp.multiply(a30, s30) // (y1y2 + 3z1z2)(y1y2 - 3z1z2
    val m43 = zp.multiply(tri30, tri31) // 9(x1x2)(x1z2 + x2z1)
    val m44 = zp.multiply(a30, s22) // (y1y2 + 3z1z2)(y1z2 + y2z1)
    val m45 = zp.multiply(tri30, s20) // 3x1x2(x1y2 + x2y1)
    // stage 5
    val s50 = zp.subtract(m40, m41)
    val a50 = zp.add(m42, m43)
    val a51 = zp.add(m44, m45)

    EcPointProj(s50, a50, a51)
  }

  // for homogeneous projective coordinates which use complete formula, pdbl is the same as padd
  def pdblHomo(p: EcPointProj) = paddHomo(p, p)

  // TODO: better implementation of ladder on ShortWeierstrassCurve
  override def padd(p0: EcPointAffine, p1: EcPointAffine) = {
    paddHomo(p0.toProjective, p1.toProjective).toAffine(Homo)
  }
}