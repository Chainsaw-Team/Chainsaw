package Chainsaw

import Chainsaw.NumericExt._
import breeze.math.Complex
import spinal.core._

import scala.language.postfixOps
import Chainsaw._
import Chainsaw.xilinx._

import scala.util.Random

/** an extension of AFix
  */
class NumericType(val maxRaw: BigInt, val minRaw: BigInt, val exp: Int) {

  val afixType = HardType(new AFix(maxRaw, minRaw, exp))

  /** -------- get core attributes from AFix in virtual global data, to assure
    * the consistency with SpinalHDL
    * --------
    */

  val (bitWidth, fractional, tempIntegral, signed, maxValue, minValue, step) =
    inVirtualGlob {
      val realInstance = afixType()
      (
        realInstance.bitWidth,
        realInstance.fracWidth,
        realInstance.intWidth, // including sign bit
        realInstance.signed,
        realInstance.maxValue,
        realInstance.minValue,
        realInstance.step
      )
    }

  val integral = tempIntegral - (if (signed) 1 else 0) // exclude sign bit

  def qFormat =
    QFormat(if (signed) bitWidth - 1 else bitWidth, fractional, signed)

  /** -------- methods using global data
    * --------
    */
  def apply() = afixType()

  def asComplex =
    HardType(
      ComplexFix(new AFix(maxRaw, minRaw, exp), new AFix(maxRaw, minRaw, exp))
    )

  def fromConstant(constant: BigDecimal): AFix = {
    val ret = apply()
    assert(
      constant <= ret.maxValue,
      s"Literal $constant is too big to be assigned in $this"
    )
    assert(
      constant >= ret.minValue,
      s"Literal $constant is too negative to be assigned in this $this"
    )
    val intValue =
      if (constant >= 0.0) (constant / step).toBigInt()
      else (constant / step).toBigInt().toBitValue(bitWidth).to2sComplement
    ret.raw := intValue
    ret
  }

  def fromConstant(constant: Double): AFix = fromConstant(BigDecimal(constant))

  def fromConstant(constant: Complex): ComplexFix = {
    val ret = asComplex()
    ret.real := constant.real
    ret.imag := constant.imag
    ret
  }

  def random = {
    val range = maxRaw - minRaw
    val validRaw = Iterator
      .continually(BigInt(bitWidth, Random))
      .dropWhile(_ > range)
      .next() + minRaw
    BigDecimal(validRaw) * BigDecimal(2).pow(exp)
  }

  // TODO: is from & to BigInt necessary?
  // TODO: is resize necessary?
  // TODO: is getRandom necessary?

  def fromAfix(afix: AFix) = new NumericType(afix.maxRaw, afix.minRaw, afix.exp)

  def typeArith(that: NumericType, op: (AFix, AFix) => AFix) = {
    inVirtualComponent(op(this.apply(), that.apply()).numericType)
  }

  // TODO: accurate type arithmetic
  //  def +(that: NumericTypeNew) = typeArith(that, _ + _)
  def +(that: NumericType) = NumericType(
    (integral max that.integral) + 1,
    fractional max that.fractional,
    signed
  )

  //  def -(that: NumericTypeNew) = typeArith(that, _ - _)
  def -(that: NumericType) = NumericType(
    (integral max that.integral) + 1,
    fractional max that.fractional,
    signed
  )

  //  def *(that: NumericTypeNew) = typeArith(that, _ * _)
  def *(that: NumericType) = NumericType(
    integral + that.integral + 1,
    fractional + that.fractional,
    signed || signed
  )

  def withCarry(bitWidth: Int) =
    NumericType(integral + bitWidth, fractional, signed)

  override def toString =
    s"${if (signed) "S" else "U"}Q${integral}_$fractional".replace("-", "N")

  def same(
      your: BigDecimal,
      golden: BigDecimal,
      absTolerance: Double,
      relativeTolerance: Double
  ) = {
    val ret =
      (your - golden).abs <= step || (your - golden).abs <= absTolerance || (your - golden).abs / (golden.abs + step) <= relativeTolerance
    if (!ret)
      logger.error(
        s"your: $your, golden: $golden, step: $step, absTolerance: $absTolerance, relativeTolerance: $relativeTolerance"
      )
    ret
  }
}

object NumericType {
  def apply(integral: Int, fractional: Int, signed: Boolean): NumericType = {
    val maxRaw = BigInt(2).pow(integral + fractional) - 1
    val minRaw =
      if (signed) -BigInt(2).pow(integral + fractional) else BigInt(0)
    new NumericType(maxRaw, minRaw, -fractional)
  }

  def apply(maxRaw: BigInt, minRaw: BigInt, exp: Int): NumericType =
    new NumericType(maxRaw, minRaw, exp)

  def apply(
      maxValue: BigDecimal,
      minValue: BigDecimal,
      exp: Int
  ): NumericType = {
    val step = BigDecimal(2).pow(exp)
    // TODO: ceil & floor for BigDecimal
    val maxRaw = (maxValue / step).toBigInt()
    val minRaw = (minValue / step).toBigInt()
    NumericType(maxRaw, minRaw, exp)
  }

  // initiation utils
  def U(integral: Int) = NumericType(integral, 0, signed = false)

  def S(integral: Int) = NumericType(integral, 0, signed = true)

  def UFix(integral: Int, fractional: Int) =
    NumericType(integral, fractional, signed = false)

  def SFix(integral: Int, fractional: Int) =
    NumericType(integral, fractional, signed = true)

  def Bool() = NumericType(1, 0, signed = false)

}
