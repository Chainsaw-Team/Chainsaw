package Chainsaw.crypto

import Chainsaw._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

import scala.util.Random

/** model of Short Weierstrass curves
  *
  * @see
  *   [[http://hyperelliptic.org/EFD/g1p/auto-shortw.html]]
  */
class EllipticCurve(
    val modulus: IntZ,
    val a: IntZ,
    val b: IntZ,
    val c: IntZ,
    val d: IntZ
) {

  val zp: Ring[IntZ] = Zp(modulus)
  val polyRing       = UnivariateRing(zp, "x")
  val curveExprX     = s"x ^ 3 + $b * x ^ 2 + $c * x + $d"
  val curveExprY     = s"y ^ 2 + $a * y"
  val curveX         = polyRing(curveExprX)
  val curveY         = polyRing(curveExprY.replace('y', 'x'))

  override def toString = s"$curveExprY = $curveExprX".dropWhile(_ == ' ')

  def isOnCurve(eccPoint: EcPointAffine) =
    if (eccPoint == EcZeroAffine) true
    else curveX.evaluate(eccPoint.x) == curveY.evaluate(eccPoint.y)

  /** naive implementation of padd & pdbl on in affine coordinates
    *
    * caution! padd & pdbl are actually two different operations in affine
    * coordinates, we merge them for simplicity while using pmul
    *
    * @see
    *   [[http://hyperelliptic.org/EFD/g1p/auto-shortw.html]]
    */
  def padd(p0: EcPointAffine, p1: EcPointAffine): EcPointAffine = {
    val (x0, x1, y0, y1) = (p0.x, p1.x, p0.y, p1.y)
    val ret = { // 5 situations in total
      if (p0 == EcZeroAffine) p1                    // situation 1
      else if (p1 == EcZeroAffine) p0               // situation 2
      else if (p0 == p1.inverse(this)) EcZeroAffine // situation 3
      else if (p0 == p1) {                          // situation 4
        val lambda = zp.divideExact(x0 * x0 * 3 + c, y0 * 2)
        val x2     = (lambda * lambda - x0 - x1) % modulus
        val y2     = (lambda * (x0 - x2) - y0) % modulus
        EcPointAffine(x2, y2)
      } else { // situation 5
        val lambda = zp.divideExact(y1 - y0, x1 - x0)
        val x2     = (lambda * lambda - x0 - x1) % modulus
        val y2     = (lambda * (x0 - x2) - y0) % modulus
        EcPointAffine(x2, y2)
      }
    }
    assert(isOnCurve(ret), s"padd/pdbl failed on $p0 + $p1")
    ret
  }

  //  def paddGraph(width: Int): Dag = null

  // caution! padd & pdbl are actually two different operations in affine coordinates, we merge them for simplicity while using pmul
  def pdbl(p: EcPointAffine) = padd(p, p)

  // scala multiplication template for different coordinates
  def multBase[T](k: IntZ, p: EcPointAffine) = {
    var temp = p
    k.toBigInt.toString(2).tail.foreach { bit =>
      temp                 = padd(temp, temp)
      if (bit == '1') temp = padd(temp, p)
    }
    temp
  }

  def pladder(
      r0: EcPointAffine,
      r1: EcPointAffine,
      bit: Int
  ): (EcPointAffine, EcPointAffine) = {
    if (bit == 1) (padd(r0, r1), pdbl(r1))
    else (pdbl(r0), padd(r0, r1))
  }

  def pmult(k: IntZ, p: EcPointAffine) = {
    var (r0, r1) = (p, pdbl(p))
    k.toBigInt.toString(2).tail.foreach { bit =>
      val pair = pladder(r0, r1, bit.asDigit)
      r0 = pair._1
      r1 = pair._2
    }
    val ret = r0
    assert(isOnCurve(ret) && ret == multBase(k, p), s"pmult failed on $k * $p")
    ret
  }

  def selfTest(points: Seq[EcPointAffine]): Unit = {
    // assert validity of testcases
    points.foreach(point =>
      assert(
        isOnCurve(point),
        s"testcases are no valid as $point is not on curve $curveExprX"
      )
    )
    logger.info("testcases are valid")
    // test padd
    points.prevAndNext { case (affine0, affine1) => padd(affine0, affine1) }
    logger.info("padd works fine")
    // test pdbl
    points.foreach(pdbl)
    logger.info("pdbl works fine")
    // test pmult
    val k = BigInt(16, Random)
    points.foreach(pmult(k, _))
    logger.info("pmult works fine")
    logger.info(s"test for curve $this passed")
  }
}

object EllipticCurve {
  def apply(
      modulus: IntZ,
      a: IntZ,
      b: IntZ,
      c: IntZ,
      d: IntZ
  ): EllipticCurve = {
    if (a.isZero && b.isZero) {
      logger.info("your curve is a short weierstrass curve")
      ShortWeierstrassCurve(modulus, c, d)
    } else {
      new EllipticCurve(modulus, a, b, c, d)
    }
  }
}

sealed trait Coordinates

object Jacobi extends Coordinates //X : Y : Z

object Homo extends Coordinates //X : Y : Z

object extendedHomo extends Coordinates //X : Y : Z : T

object extendedJacobi extends Coordinates //X : Y : Z : W

object extendedAffine extends Coordinates //x : y : u

object EcZeroAffine extends EcPointAffine(null, null)

case class EcPointAffine(x: IntZ, y: IntZ) {

  def toProjective(
      coordinates: Coordinates
  )(implicit eccGroup: EllipticCurve): EcPointProj = {
    coordinates match {
      case crypto.Jacobi => EcPointProj(x, y, asBigInteger(1))(coordinates)
      case crypto.Homo   => EcPointProj(x, y, asBigInteger(1))(coordinates)
      case crypto.extendedHomo =>
        EcPointProj(x, y, 1, eccGroup.zp.multiply(x, y))(coordinates)
      case crypto.extendedJacobi =>
        EcPointProj(x, y, asBigInteger(1), asBigInteger(1))(coordinates)
      case crypto.extendedAffine =>
        EcPointProj(
          x,
          y,
          eccGroup.zp.multiply(
            x,
            y,
            asBigInteger(
              BigInt(
                "0196bab03169a4f2ca0b7670ae65fc7437786998c1a32d217f165b2fe0b32139735d947870e3d3e4e02c125684d6e016",
                16
              )
            )
          )
        )(coordinates)
    }
  }

  def +(that: EcPointAffine)(implicit eccGroup: EllipticCurve) =
    eccGroup.padd(this, that)

  def dbl(implicit eccGroup: EllipticCurve) = eccGroup.pdbl(this)

  def *(that: IntZ)(implicit eccGroup: EllipticCurve) =
    eccGroup.pmult(that, this)

  def inverse(implicit eccGroup: EllipticCurve) =
    EcPointAffine(x, (-y) % eccGroup.modulus)
}

/** ecliptic curve point in projective(Jacobi or homogeneous) coordinates
  */
case class EcPointProj(axis: IntZ*)(coordinates: Coordinates) {

  def toAffine(implicit eccGroup: EllipticCurve) = {
    if (isZero) EcZeroAffine
    else {
      val denominators = coordinates match {
        case crypto.Jacobi         => (axis(2).pow(2), axis(2).pow(3))
        case crypto.Homo           => (axis(2), axis(2))
        case crypto.extendedHomo   => (axis(2), axis(2))
        case crypto.extendedJacobi => (axis(2), axis(3))
        case crypto.extendedAffine => (asBigInteger(1), asBigInteger(1))
      }
      val xAffine = eccGroup.zp.divideExact(axis(0), denominators._1)
      val yAffine = eccGroup.zp.divideExact(axis(1), denominators._2)
      EcPointAffine(xAffine, yAffine)
    }
  }

  def isZero = {
    coordinates match {
      case crypto.Jacobi         => axis(2).isZero
      case crypto.Homo           => axis(2).isZero
      case crypto.extendedHomo   => axis(2).isZero
      case crypto.extendedJacobi => axis(2).isZero || axis(3).isZero
      case crypto.extendedAffine => false
    }
  }
}

object Temp extends App {
  import sys._

  println(sys.env("HOME"))
  println(sys.env("VIVADO"))

}
