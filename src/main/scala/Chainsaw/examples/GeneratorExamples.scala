package Chainsaw.examples

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random
import Chainsaw.memory._

case class ExampleAdder(width: Int) extends ChainsawOperatorGenerator {
  override def impl(data: Seq[BigDecimal]) = Seq(data.sum)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = yours.equals(golden)

  override def testCases = Seq.fill(1000)(BigInt(width, Random)).map(BigDecimal(_))

  override def latency = 1

  override def name = s"adder_w$width"

  override def vivadoUtilEstimation = VivadoUtilEstimation(lut = width)

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq.fill(2)(NumericTypeNew.U(width))

  override def outputTypes = Seq(NumericTypeNew.U(width + 1))

  override def implH = new ChainsawOperatorModule(this) {
    dataOut.head := dataIn.reduce(_ + _).d()
  }

  override def implNaiveH = None
}

case class ExampleAddSub(width: Int) extends ChainsawDynamicOperatorGenerator {
  override def impl(testCase: TestCase) = {
    val sub = testCase.control.head.toInt == 1
    val ret = if (sub) testCase.data.reduce(_ - _) else testCase.data.sum
    Seq(ret)
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = {
    Seq.fill(10)(TestCase(randomInputVector, Seq(BigDecimal(1)))) ++ // for sub
      Seq.fill(10)(TestCase(randomInputVector, Seq(BigDecimal(0)))) // for add
  }

  override def latency(control: Seq[BigDecimal]) = 1

  override def controlTypes = Seq(NumericTypeNew.Bool())

  override def implH = new ChainsawDynamicOperatorModule(this) {
    val sub = controlIn.head.asBits.asBool
    when(sub.d()) {
      dataOut.head := dataIn.reduce(_ - _).d()
    } otherwise {
      dataOut.head := dataIn.reduce(_ + _).d()
    }
    validOut := validIn.validAfter(1)
  }

  override def implNaiveH = None

  override def name = s"addsub_w$width"

  override def vivadoUtilEstimation = VivadoUtilEstimation(lut = width)

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq.fill(2)(NumericTypeNew.S(width))

  logger.info(s"input types: ${inputTypes.mkString(" ")}")

  override def outputTypes = Seq(NumericTypeNew.S(width + 1))
}

case class ExampleStaticFlip(dataType: NumericTypeNew, length: Int) extends ChainsawFrameGenerator {

  override def name = s"flip_$length"

  override def impl(data: Seq[BigDecimal]) = data.reverse

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = yours.equals(golden)

  override def testCases = Seq.fill(1000)(randomInputFrame)

  override def latency = period + 1

  override def inputFrameFormat = MatrixFormat(1, length)

  override def outputFrameFormat = MatrixFormat(1, length)

  override def implH = new ChainsawFrameModule(this) {
    val paddedLength = nextPow2(period).toInt
    val ram = Mem(HardType(dataIn), paddedLength << 1)

    val counterWrite = Counter(length)
    val counterRead = Counter(length)
    val doWrite = validIn || counterWrite =/= U(0)
    val writeFinish = RegNext(counterWrite.willOverflow, init = False)
    val doRead = writeFinish || counterRead =/= U(0)

    when(doWrite)(counterWrite.increment())
    when(doRead)(counterRead.increment())

    val pingPongPointer = RegInit(False)
    when(counterWrite.willOverflow)(pingPongPointer := ~pingPongPointer)

    val writeAddr: UInt = pingPongPointer.asUInt @@ counterWrite.value
    val readAddr: UInt = ~pingPongPointer.asUInt @@ (U(length - 1) - counterRead)

    ram.write(writeAddr, dataIn, doWrite)
    dataOut := ram.readSync(readAddr)
    lastOut := counterRead.willOverflow.d()
  }

  override def implNaiveH = None

  override def vivadoUtilEstimation = VivadoUtilEstimation()

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(dataType)
}

case class ExampleDynamicFlip(dataType: NumericTypeNew, maxLength: Int)
  extends ChainsawDynamicFrameGenerator {

  val innerMaxLength = maxLength + 2 // for FIFO latency

  override def name = s"flip_$maxLength"

  override def impl(testCase: TestCase) = testCase.data.reverse

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = yours.equals(golden)

  override def testCases =
    Seq.fill(100)(TestCase(randomInputFrame(Seq(BigDecimal(10))), Seq(BigDecimal(10)))) ++ // for sub
      Seq.fill(100)(TestCase(randomInputFrame(Seq(BigDecimal(20))), Seq(BigDecimal(20)))) // for add

  override def latency(control: Seq[BigDecimal]) = innerMaxLength + 1

  override def resetCycle = 0

  override def inputFrameFormat(control: Seq[BigDecimal]) = MatrixFormat(1, control.head.toInt)

  override def outputFrameFormat(control: Seq[BigDecimal]) = MatrixFormat(1, control.head.toInt)

  override def implH = new ChainsawDynamicFrameModule(this) {

    val paddedLength = nextPow2(innerMaxLength).toInt
    val ram = Mem(HardType(dataIn), paddedLength << 1)

    // writing logic
    val counterWrite = Counter(paddedLength << 1, inc = validIn)
    ram.write(counterWrite.value, dataIn, validIn)

    val control = controlIn.head.asUInt
    val counterWriteDone = DynamicCounter(control)
    val doWrite = validIn || counterWriteDone =/= U(0)
    when(doWrite)(counterWriteDone.increment())
    val writeDone = counterWriteDone.willOverflow

    // reading logic
    val lastControl = control.d(innerMaxLength)
    val counterRead = DynamicCounter(lastControl)
    val readValid = validIn.validAfter(innerMaxLength)
    val doRead = readValid || counterRead =/= U(0)
    when(doRead)(counterRead.increment())
    val readDone = counterRead.willOverflow && readValid

    val addrFifo = StreamFifo(UInt(log2Up(paddedLength << 1) bits), innerMaxLength)
    addrFifo.io.push.valid := writeDone
    addrFifo.io.push.payload := counterWrite
    addrFifo.io.pop.ready := readDone
    val readAddrTop = addrFifo.io.pop.payload

    val readAddr: UInt = readAddrTop - counterRead
    dataOut := ram.readSync(readAddr)

    validOut := validIn.validAfter(innerMaxLength + 1)
    lastOut := readDone.d()
  }

  override def implNaiveH = None

  override def vivadoUtilEstimation = VivadoUtilEstimation()

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(dataType)

  override def controlTypes = Seq(NumericTypeNew.U(log2Up(innerMaxLength)))
}

case class ExampleStaticFir(dataType: NumericTypeNew, coeffs: Seq[Double])
  extends ChainsawInfiniteGenerator {

  val productType = dataType * dataType

  override def impl(data: Seq[BigDecimal]) = {
    val dataWithZeros = data ++ Seq.fill(coeffs.length - 1)(BigDecimal(0))
    dataWithZeros.sliding(coeffs.length).map { window =>
      window.zip(coeffs.reverse).map { case (d, c) => d * BigDecimal(c) }.sum
    }
  }.toSeq

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.zip(golden).forall { case (y, g) => dataType.same(y, g, 1e-1, 1e-1) }

  override def testCases = {
    val ret = Seq(10, 20, 30, 20, 10).map(i => Seq.fill(i)(dataType.random))
    logger.info(s"testCases: ${ret.head.mkString(" ")}")
    ret
  }

  override def latency = 2 * (coeffs.length + 1) + 1

  override def resetCycle = coeffs.length

  override def implH = new ChainsawInfiniteModule(this) {
    val x = Mux(validIn, dataIn.head, dataType.fromConstant(0))
    val xline = Seq.iterate(x.d(2), coeffs.length)(_.d(2))
    val preAdded = xline
    val scaled = preAdded.zip(coeffs).map { case (port, coeff) => (port * dataType.fromConstant(coeff).d()).d() }
    val zero = productType.fromConstant(0.0)
    // the first element is a dummy, it is a must for extreme fmax, or PREG won't be used for the first DSP
    val ret = (zero +: scaled).reduce((a, b) => (a +| b).d()) // addition without width growth
    dataOut.head := ret.d()
  }

  override def implNaiveH = None

  override def name = "Fir"

  override def vivadoUtilEstimation = VivadoUtilEstimation(dsp = coeffs.length)

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(productType)
}


case class ExampleDynamicFir(dataType: NumericTypeNew, tap: Int)
  extends ChainsawDynamicInfiniteGenerator {

  override def impl(testCase: TestCase) = {
    val TestCase(data, coeffs) = testCase
    val dataWithZeros = data ++ Seq.fill(coeffs.length - 1)(BigDecimal(0))
    dataWithZeros.sliding(coeffs.length).map { window =>
      window.zip(coeffs.reverse).map { case (d, c) => d * c }.sum
    }
  }.toSeq

  val productType = dataType * dataType

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.zip(golden).forall { case (y, g) => dataType.same(y, g, 1e-1, 1e-1) }

  override def testCases = {
    val data = Seq(10, 20, 30, 20, 10).map(i => Seq.fill(i)(dataType.random))
    data.map(TestCase(_, randomControl))
  }

  override def controlTypes = Seq.fill(tap)(dataType)

  override def latency(control: Seq[BigDecimal]) = 2 * (tap + 1) + 1

  override def resetCycle = 2 * (tap + 1) + 1

  override def implH = new ChainsawDynamicInfiniteModule(this) {
    val x = Mux(validIn, dataIn.head, dataType.fromConstant(0))
    val coeffs = RegNextWhen(controlIn, validIn.rise())
    val xline = Seq.iterate(x.d(2), coeffs.length)(_.d(2))
    val preAdded = xline
    val scaled = preAdded.zip(coeffs).map { case (port, coeff) => (port * coeff.d()).d() }
    val zero = productType.fromConstant(0.0)
    // the first element is a dummy, it is a must for extreme fmax, or PREG won't be used for the first DSP
    val ret = (zero +: scaled).reduce((a, b) => (a +| b).d()) // addition without width growth
    dataOut.head := ret.d()
    validOut := validIn.validAfter(2 * (tap + 1) + 1)
  }

  override def implNaiveH = None

  override def name = "DynamicFir"

  override def vivadoUtilEstimation = VivadoUtilEstimation(dsp = tap)

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(productType)
}


object TestGeneratorExamples extends App {
  ChainsawOperatorTest("testAdder", ExampleAdder(8))
  ChainsawDynamicOperatorTest("testAdder", ExampleAddSub(8), terminateAfter = 1000)
  ChainsawFrameTest("testFlip", ExampleStaticFlip(dataType = NumericTypeNew.U(8), length = 20), terminateAfter = 1000)
  ChainsawDynamicFrameTest("testFlip", ExampleDynamicFlip(dataType = NumericTypeNew.U(8), maxLength = 20), terminateAfter = 1000)
  ChainsawInfiniteTest("testFir", ExampleStaticFir(dataType = NumericTypeNew.SFix(0, 14), Seq.fill(5)(Random.nextDouble())), terminateAfter = 1000)
  ChainsawDynamicInfiniteTest("testFir", ExampleDynamicFir(dataType = NumericTypeNew.SFix(0, 14), 5), terminateAfter = 1000)
}