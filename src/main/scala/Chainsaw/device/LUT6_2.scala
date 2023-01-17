package Chainsaw.device

import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import Chainsaw._
import Chainsaw.xilinx._

case class LUT6_2(init: BigInt) extends Unisim {
  val generic: Generic = new Generic {
    val INIT = B(init, 64 bits)
  }
  val I0, I1, I2, I3, I4, I5 = in Bool ()
  val O5, O6                 = out Bool ()
  addPrimitive("LUT6_2")
}

/** generate a LUT6_2 with 2 5-input logic expression
  */
case class LUT5to2(
    expressionO5: (Boolean, Boolean, Boolean, Boolean, Boolean) => Boolean,
    expressionO6: (Boolean, Boolean, Boolean, Boolean, Boolean) => Boolean
) extends ChainsawOperatorGenerator
    with FixedLatency {

  override def inputTypes = Seq.fill(5)(NumericType.Bool())

  override def outputTypes = Seq.fill(2)(NumericType.Bool())

  override def impl(testCase: TestCase) = {
    val Seq(i0, i1, i2, i3, i4) = testCase.data.map(_.toInt).map(_ == 1)
    val o5                      = expressionO5(i0, i1, i2, i3, i4)
    val o6                      = expressionO6(i0, i1, i2, i3, i4)
    Seq(o5, o6).map(_.toInt).map(BigDecimal(_))
  }

  val exp0 = (data: Seq[Boolean]) => {
    val Seq(i0, i1, i2, i3, i4) = data
    expressionO5(i0, i1, i2, i3, i4)
  }

  val exp1 = (data: Seq[Boolean]) => {
    val Seq(i0, i1, i2, i3, i4) = data
    expressionO6(i0, i1, i2, i3, i4)
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = Seq.fill(100)(randomTestCase)

  val init =
    (LUT6_2.expression2value(exp1, 5) << 32) + LUT6_2.expression2value(exp0, 5)
  logger.info(s"LUT value = ${init.toString(16)}")

  override def implH = new ChainsawOperatorModule(this) {
    val core = LUT6_2(init)
    core.I0        := dataIn(0).raw.asBool
    core.I1        := dataIn(1).raw.asBool
    core.I2        := dataIn(2).raw.asBool
    core.I3        := dataIn(3).raw.asBool
    core.I4        := dataIn(4).raw.asBool
    core.I5        := True
    dataOut(0).raw := core.O5.asBits
    dataOut(1).raw := core.O6.asBits
  }

  override def implNaiveH = None

  override def latency() = 0

  override def name = s"LUT5_2_${init.toString(16)}"

  override def vivadoUtilEstimation = VivadoUtil(lut = 1, ff = 0)

  override def fmaxEstimation = 800 MHz

  def process(bools: Bool*) = {
    val core = getImplH
    core.dataIn := bools.map(_.asUInt.toAFix)
    core.dataOut.map(_.raw.asBool)
  }
}

object LUT6_2 {

  def process(input: Seq[Bool], init: BigInt): Seq[Bool] = {
    val lut = LUT6_2(init)
    lut.I0 := input(0)
    lut.I1 := input(1)
    lut.I2 := input(2)
    lut.I3 := input(3)
    lut.I4 := input(4)
    lut.I5 := input(5)
    Seq(lut.O5, lut.O6)
  }

  def expression2value(expression: Seq[Boolean] => Boolean, bitCount: Int) = {
    val truthTable = ArrayBuffer[Int]()
    (0 until (1 << bitCount)) // build truth table
      .map { id =>
        val bools = id.toBinaryString.reverse.padTo(bitCount, '0').map(_ == '1')
        bools
      } // low to high
      .map(expression)
      .foreach(bool => truthTable += bool.toInt)

    BigInt(truthTable.reverse.mkString, 2)
  }

  def getExpressionWithInverse(
      expression: Seq[Boolean] => Boolean,
      inverseList: Seq[Boolean]
  ): Seq[Boolean] => Boolean = { data =>
    {
      val inversed = data.zip(inverseList).map { case (bool, inverse) =>
        if (inverse) !bool else bool
      }
      expression(inversed)
    }
  }

  def getValueWithInverse(value: BigInt, inverseList: Seq[Boolean]): BigInt = {
    val truthTable = value.toString(2).reverse.padTo(64, '0') // low to high
    val inversedTruthTable = (0 until 64).map { addr =>
      val addrStr = addr.toBinaryString.reverse
        .padTo(6, '0')
        .zip(inverseList)
        .map { case (bit, inverse) =>
          if (inverse) 1 - bit.asDigit else bit.asDigit
        }
        .reverse
        .mkString
      val inversedAddr = BigInt(addrStr, 2)
      truthTable(inversedAddr.toInt)
    } // low to high
    val ret = BigInt(inversedTruthTable.reverse.mkString, 2)
    ret
  }

  def main(args: Array[String]): Unit = {
    val exp0 = (data: Seq[Boolean]) => {
      val Seq(i0, i1, i2, i3, i4) = data
      (i0.toInt + i1.toInt + i2.toInt) >= 2
    }
    val exp1 = (data: Seq[Boolean]) => {
      val Seq(i0, i1, i2, i3, i4) = data
      i0 ^ i1 ^ i2 ^ i3 ^ i4 // sum bit of FA
    }
    println(expression2value(exp0, 5).toString(16))
    println(
      expression2value(
        getExpressionWithInverse(exp1, Seq(false, false, true, false, false)),
        5
      ).toString(16) + expression2value(
        getExpressionWithInverse(exp0, Seq(false, false, true, false, false)),
        5
      ).toString(16)
    )
    println(
      expression2value(
        getExpressionWithInverse(exp1, Seq(false, true, true, false, false)),
        5
      ).toString(16) + expression2value(
        getExpressionWithInverse(exp0, Seq(false, true, true, false, false)),
        5
      ).toString(16)
    )
  }
}
