package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core.U

trait UnsignedMerge
    extends ChainsawOperatorGenerator
    with Unaligned
    with FixedLatency {

  def arithInfos: Seq[ArithInfo]

  def outputArithInfos: Seq[ArithInfo]

  def weightLow = arithInfos.map(_.weight).min

  require(outputArithInfos.forall(_.weight == weightLow))

  val timeMin = arithInfos.map(_.time).min

  override def inputTimes = arithInfos.map(_.time - timeMin)

  override def outputTimes = Seq(0)

  override def inputTypes = arithInfos.map(_.width).map(NumericType.U)

  def positiveLength: Int

  override def impl(testCase: TestCase) = {
    val ret = testCase.data
      .map(_.toBigInt())
      .zip(arithInfos)
      .map { case (data, info) => info.eval(data) }
      .sum
    Seq(BigDecimal(ret)).padTo(outputTypes.length, BigDecimal(0))
  }

  // FIXME: output according to outputArithInfos
  override def implNaiveH =
    Some(new ChainsawOperatorModule(this) {
      val ops = dataIn.zip(inputTimes).map { case (op, time) =>
        op.d(inputInterval - time)
      }
      val opAndInfos = ops.map(_.asUInt()).zip(arithInfos)
      val positive = opAndInfos
        .filter(_._2.isPositive)
        .map { case (int, info) => int << info.weight }
        .reduce(_ +^ _)
      val negative =
        if (arithInfos.exists(!_.isPositive))
          opAndInfos
            .filterNot(_._2.isPositive)
            .map { case (int, info) => int << info.weight }
            .reduce(_ +^ _)
        else U(0)
      val ret = (positive - negative).d(latency() - inputInterval)
      dataOut.head := ret.toAFix.truncated
      dataOut.tail.foreach(port => port := port.getZero)
    })

  override def testCases = Seq.fill(100)(TestCase(randomDataVector))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    if (golden.exists(_ < 0)) {
      logger.info("negative sum occur in your testcase")
      true
    } // skip
    else {
      val yourRaw = yours
        .map(_.toBigInt())
        .zip(outputArithInfos)
        .map { case (int, info) => info.eval(int) }
        .sum
      val goldenRaw = golden.map(_.toBigInt()).sum
      val yourSum   = yourRaw.mod(pow2(positiveLength + weightLow))
      val goldenSum = goldenRaw.mod(pow2(positiveLength + weightLow))
      if (yourSum != goldenSum)
        logger.info(
          s"diff = ${(yourSum - goldenSum).toString(16)} = ${yourSum - goldenSum}"
        )
      yourSum == goldenSum
    }
  }
}
