package Chainsaw

import spinal.core._

import scala.language.postfixOps
import NumericExt._

class ComplexFix(maxRaw: BigInt, minRaw: BigInt, exp: Int)
  extends Bundle {

  val real, imag = new AFix(maxRaw, minRaw, exp)

  override def clone() = new ComplexFix(maxRaw, minRaw, exp)

  def +(that: ComplexFix): ComplexFix = ComplexFix(real + that.real, imag + that.imag)

  def +|(that: ComplexFix): ComplexFix = ComplexFix(real +| that.real, imag +| that.imag)

  def -(that: ComplexFix): ComplexFix = ComplexFix(real - that.real, imag - that.imag)

  def -|(that: ComplexFix): ComplexFix = ComplexFix(real -| that.real, imag -| that.imag)

  /** --------
   * multiplication
   * -------- */

  def *(that: AFix): ComplexFix = ComplexFix(real * that, imag * that)

  // TODO: multiplication using 3 mults

  /** --------
   * nontrivial computations
   * -------- */

  def >>(that: Int) = ComplexFix(real >> that, imag >> that)

  def <<(that: Int) = ComplexFix(real << that, imag << that)

  def unary_-(): ComplexFix = ComplexFix(-real, -imag)

  def multiplyI = ComplexFix(-imag, real)

  def divideI = ComplexFix(imag, -real)

  def conj = ComplexFix(real, imag.symmetricNegate)

  // TODO: truncation by real/complex numeric type
  def truncate(numericType: NumericType) = ???

  // TODO: saturation by real/complex numeric type
  def saturate(numericType: NumericType) = ???

  // FIXME: use AFix instead of NumericType
  def fixTo(numericType: NumericType) = ComplexFix(real.fixTo(numericType()), imag.fixTo(numericType()))
}

object ComplexFix {

  def apply(maxRaw: BigInt, minRaw: BigInt, exp: ExpNumber) = new ComplexFix(maxRaw, minRaw, exp.value)

  def apply(real: AFix, imag: AFix) = {
    val ret = new ComplexFix(real.maxRaw, real.minRaw, real.exp)
    ret.real := real
    ret.imag := imag
    ret
  }

  def apply(amplitude: ExpNumber, resolution: ExpNumber, signed: Boolean = true) = {
    val maxRaw = BigInt(2).pow(amplitude.value - resolution.value) - 1
    val minRaw = if (signed) -BigInt(2).pow(amplitude.value - resolution.value) else BigInt(0)
    new ComplexFix(maxRaw, minRaw, resolution.value)
  }
}