package Chainsaw

import breeze.linalg.max
import breeze.math._
import breeze.numerics._
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

sealed trait NumericEnum

object UIntType extends NumericEnum

object SIntType extends NumericEnum

object SFixType extends NumericEnum

object ComplexFixType extends NumericEnum

/** Chainsaw version HardType which can be manipulated without a hardware context
 *
 * @param integral   width of integral part
 * @param fractional width of fractional part
 * @param signed     signed when it is true, unsigned when it is false
 * @param complex    complex number when it is true, real number when it is fals
 */
case class NumericType(integral: Int, fractional: Int, signed: Boolean, complex: Boolean) {

  /** --------
   * requirements
   * -------- */
  require(integral >= 0 && fractional >= 0)
  if (complex) require(signed, "unsigned complex is not supported")
  else if (fractional > 0) require(signed, "unsigned fixed is not supported")

  val numericEnum =
    if (complex) ComplexFixType
    else if (fractional > 0) SFixType
    else if (signed) SIntType
    else UIntType

  def isUInt = numericEnum == UIntType

  def isSInt = numericEnum == SIntType

  def isSFix = numericEnum == SFixType

  def isComplexFix = numericEnum == ComplexFixType

  def bitWidth = ((if (signed) 1 else 0) + integral + fractional) * (if (complex) 2 else 1)

  /** --------
   * NumericTypeInfo -> HardType
   * -------- */
  def asUInt: HardType[UInt] = {
    require(numericEnum == UIntType)
    HardType(UInt(integral bits))
  }

  def asSInt: HardType[SInt] = {
    require(numericEnum == SIntType)
    HardType(SInt(integral bits))
  }

  def asSFix: HardType[SFix] = {
    require(numericEnum == SFixType)
    HardType(SFix(integral exp, -fractional exp))
  }

  def asComplexFix: HardType[ComplexFix] = {
    require(numericEnum == ComplexFixType, s"your type: $numericEnum")
    HardType(ComplexFix(integral exp, -fractional exp))
  }

  def toSFixInfo = NumericType(integral, fractional, signed, complex = false)

  def toComplexFixInfo = NumericType(integral, fractional, signed, complex = true)

  /** --------
   * constant -> signal
   * -------- */
  def fromConstant(constant: Double) = {
    require(numericEnum == SFixType)
    val sfType = asSFix()
    SF(constant, sfType.maxExp exp, sfType.minExp exp)
  }

  def fromConstant(constant: Complex) = {
    require(numericEnum == ComplexFixType)
    val sfType = asSFix()
    val real = SF(constant.real, sfType.maxExp exp, sfType.minExp exp)
    val imag = SF(constant.imag, sfType.maxExp exp, sfType.minExp exp)
    ComplexFix(real, imag)
  }

  /** --------
   * BigInt -> others TODO: verification
   * -------- */
  private def toSigned(bigInt: BigInt, width: Int): BigInt = {
    val padded = bigInt.toString(2).padToLeft(width + 1, '0')
    val sign = if (padded.head == '1') -1 else 0
    BigInt(1 << width) * sign + BigInt(padded.tail, 2)
  }

  def bits2SInt(bigInt: BigInt) = toSigned(bigInt, integral)

  def bits2SFix(bigInt: BigInt): Double = (BigDecimal(toSigned(bigInt, integral + fractional)) / BigDecimal(BigInt(1) << fractional)).toDouble

  def bits2ComplexFix(bigInt: BigInt): Complex = {
    val imag = bigInt >> (bitWidth / 2)
    val real = bigInt % (1 << (bitWidth / 2)) // real is the first one in ComplexFix, so it takes the lower part
    Complex(bits2SFix(real), bits2SFix(imag))
  }

  def fromBigInt(bigInt: BigInt) = numericEnum match {
    case UIntType => bigInt
    case SIntType => bits2SInt(bigInt)
    case SFixType => bits2SFix(bigInt)
    case ComplexFixType => bits2ComplexFix(bigInt)
  }

  /** --------
   * others -> BigInt
   * -------- */
  private def toUnSigned(sint: BigInt, width: Int): BigInt = if (sint < 0) sint + (BigInt(1) << (width + 1)) else sint

  def sint2Bits(sint: BigInt): BigInt = toUnSigned(sint, integral)

  // TODO: using BigDecimal
  def double2Bits(double: Double): BigInt = {
    val sint = round(double * (1 << fractional)).toInt
    toUnSigned(sint, integral + fractional)
  }

  def complex2Bits(complex: Complex) = {
    val sintR = double2Bits(complex.real)
    val sintI = double2Bits(complex.imag)
    BigInt(sintI.toString(2).padToLeft(bitWidth / 2, '0') + sintR.toString(2).padToLeft(bitWidth / 2, '0'), 2)
  }

  def toBigInt(value: Any) = numericEnum match {
    case UIntType => value.asInstanceOf[BigInt]
    case SIntType => sint2Bits(value.asInstanceOf[BigInt])
    case SFixType => double2Bits(value.asInstanceOf[Double])
    case ComplexFixType => complex2Bits(value.asInstanceOf[Complex])
  }

  /** --------
   * Bits -> others
   * -------- */
  def toComplex(bits: Bits): ComplexFix = {
    val ret = asComplexFix()
    ret.assignFromBits(bits)
    ret
  }

  /** --------
   * resize/truncate
   * -------- */
  def resize(bits: Bits) = {
    if (bits.getBitsWidth > bitWidth) logger.warn(s"${bits.getBitsWidth} bits -> $bitWidth bits")
    numericEnum match {
      case UIntType => if (bits.getBitsWidth > bitWidth) None else Some(bits.resize(bitWidth))
      case SIntType => if (bits.getBitsWidth > bitWidth) None else Some(bits.resize(bitWidth))
    }
  }

  // TODO: sfixinfo should be able to represent numbers with maxExp less than 0

  def maxValue = numericEnum match {
    case UIntType => Pow2(integral) - 1
    case SIntType => Pow2(integral) - 1
    case SFixType => Pow2(integral).toDouble - (1.0 / Pow2(fractional).toDouble)
    case ComplexFixType => ???
  }

  def minValue = numericEnum match {
    case UIntType => BigInt(0)
    case SIntType => -Pow2(integral)
    case SFixType => -Pow2(integral).toDouble + 1.0 - (1.0 / Pow2(fractional).toDouble)
    case ComplexFixType => ???
  }

  def resolution = numericEnum match {
    case UIntType => 1.0
    case SIntType => 1.0
    case SFixType => 1.0 / Pow2(fractional).toDouble
    case ComplexFixType => 1.0 / Pow2(fractional).toDouble
  }

  /** --------
   * random
   * -------- */
  def getRandom = numericEnum match {
    case UIntType => BigInt(integral, Random)
    case SIntType => BigInt(integral + 1, Random) - Pow2(integral)
    case SFixType =>
      val upper = maxValue.asInstanceOf[Double]
      Random.nextDouble() * 2 * upper - upper
    case ComplexFixType =>
      val upper = toSFixInfo.maxValue.asInstanceOf[Double]
      Complex(Random.nextDouble() * 2 * upper - upper, Random.nextDouble() * 2 * upper - upper)
  }

  /** --------
   * type inference
   * -------- */
  def *(that: NumericType): NumericType = {
    require(this.numericEnum == that.numericEnum, "type arithmetic is allowed only between same types")
    numericEnum match {
      case UIntType => UIntInfo(this.integral + that.integral)
      case SIntType => SIntInfo(this.integral + that.integral + 1)
      case SFixType => SFixInfo(this.integral + that.integral + 1, this.fractional + that.fractional)
      case ComplexFixType => ComplexFixInfo(this.integral + that.integral + 1, this.fractional + that.fractional)
    }
  }

  // TODO: verification
  def +(that: NumericType) = {
    require(this.numericEnum == that.numericEnum, "type arithmetic is allowed only between same types")
    numericEnum match {
      case UIntType => UIntInfo(max(this.integral, that.integral))
      case SIntType => SIntInfo(max(this.integral, that.integral))
      case SFixType => SFixInfo(max(this.integral, that.integral), max(this.fractional, that.fractional))
      case ComplexFixType => ComplexFixInfo(max(this.integral, that.integral), max(this.fractional, that.fractional))
    }
  }

  def +^(that: NumericType) = {
    require(this.numericEnum == that.numericEnum, "type arithmetic is allowed only between same types")
    numericEnum match {
      case UIntType => UIntInfo(max(this.integral, that.integral) + 1)
      case SIntType => SIntInfo(max(this.integral, that.integral) + 1)
      case SFixType => SFixInfo(max(this.integral, that.integral) + 1, max(this.fractional, that.fractional))
      case ComplexFixType => ComplexFixInfo(max(this.integral, that.integral) + 1, max(this.fractional, that.fractional))
    }
  }

  override def toString = {
    s"typeInfo: ${numericEnum.getClass.getSimpleName} with integral=$integral, fractional=$fractional, width=$bitWidth"
  }
}

/** --------
 * entrance
 * -------- */

object UIntInfo {
  def apply(integral: Int) = NumericType(integral, 0, signed = false, complex = false)
}

object SIntInfo {
  def apply(integral: Int) = NumericType(integral, 0, signed = true, complex = false)
}

object SFixInfo {
  def apply(integral: Int, fractional: Int) =
    NumericType(integral, fractional, signed = true, complex = false)
}

object ComplexFixInfo {
  def apply(integral: Int, fractional: Int) =
    NumericType(integral = integral, fractional = fractional, signed = true, complex = true)
}


object ShowNumericTypeInfo extends App {

  val complex = ComplexFixInfo(3, 5) // using complex as an example as it is the most complicated type
  println(complex)
  // for design TODO: add user case

  // for simulation
  val raw = BigInt(complex.bitWidth, Random)
  println(s"raw = $raw")
  val typed = complex.fromBigInt(raw)
  println(s"complex.fromBits(raw) = $typed") // for simulation, BigInt -> Complex
  println(s"complex.toBits(typed) = ${complex.toBigInt(typed)}") // for simulation, Complex -> BigInt
}