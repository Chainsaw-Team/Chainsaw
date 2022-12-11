package Chainsaw.deprecated

import spinal.core._

import scala.language.postfixOps

/** complex number type based on SFix,Bundle and Breeze
 *
 */
case class ComplexFix(peak: ExpNumber, resolution: ExpNumber) extends Bundle {

  val real = SFix(peak, resolution)
  val imag = SFix(peak, resolution)

  val maxExp = peak.value
  val minExp = resolution.value

  val numericType: NumericType = ComplexFixInfo(maxExp, -minExp)

  /** --------
   * addition
   * -------- */

  def +(that: ComplexFix): ComplexFix = ComplexFix(real + that.real, imag + that.imag)

  def +^(that: ComplexFix): ComplexFix = ComplexFix(real +^ that.real, imag +^ that.imag)

  def -(that: ComplexFix): ComplexFix = ComplexFix(real - that.real, imag - that.imag)

  def -^(that: ComplexFix): ComplexFix = ComplexFix(real -^ that.real, imag -^ that.imag)

  /** --------
   * multiplication
   * -------- */

  def *(that: SFix): ComplexFix = {
    val R = real * that
    val I = imag * that

    ComplexFix(R, I)
  }

  // TODO: multiplication using 3 mults
  //  def *(that: ComplexFix): ComplexFix = ???

  /** --------
   * nontrivial computations
   * -------- */

  def >>(that: Int) = ComplexFix(real >> that, imag >> that)

  def <<(that: Int) = ComplexFix(real << that, imag << that)

  def unary_-(): ComplexFix = ComplexFix(-real, -imag)

  def multiplyI = ComplexFix(-imag, real)

  def divideI = ComplexFix(imag, -real)

  def conj = ComplexFix(real, -imag)

  /** --------
   * truncation
   * -------- */

  def truncate(complexInfo: NumericType) = {
    val retReal, retImag = complexInfo.toSFixInfo.asSFix()
    retReal := real.truncated
    retImag := imag.truncated
    ComplexFix(retReal, retImag)
  }
}

object ComplexFix {
  def apply(R: SFix, I: SFix): ComplexFix = {
    require(R.numericType == I.numericType)
    val ret = R.numericType.toComplexFixInfo.asComplexFix()
    ret.real := R
    ret.imag := I
    ret
  }
}