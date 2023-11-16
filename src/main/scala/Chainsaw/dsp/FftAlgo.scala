package Chainsaw.dsp

import Chainsaw._
import Chainsaw.io.pythonIo._
import Chainsaw.edaFlow.vivado.VivadoUtil
import spinal.core._

import java.io.File
import scala.util.Random

abstract class FftAlgo extends HardAlgo {

  val n: Int

//  def fft(data: Signal) = {
//    exportSignal(data)
//    runPython(new File("utils/fft.py"))
//    importSignal().head
//  }

  // TODO: NTT

  def impl(data: Signal): Signal

//  def selfTest(): Unit = {
//    val testCase = Seq.fill(10)(Seq.fill(n)(BigDecimal(Random.nextDouble())))
//    val golden   = testCase.map(fft)
//    val yours    = testCase.map(impl)
//    assert(golden.zip(yours).forall { case (signal0, signal1) => corrMetric(signal0, signal1, 0.9) })
//  }
}

case class CooleyTukeyFftAlgo(override val n: Int) extends FftAlgo {
  override def impl(data: Signal): Signal = data

  override def vivadoUtilEstimation: VivadoUtil = ???
}

//object Algos extends App {
//
//  def omega(index: Int)(implicit N: Int) = exp(-i * 2 * Pi * index / N) // \omega ^{ik}
//  def beta(index: Int)(implicit N: Int)  = exp(-i * Pi * index / N)     // \omega ^{ik}
//
//  def bluesteinChirp(input: DenseVector[BComplex]) = {
//    implicit val N                   = input.length
//    val data: DenseVector[BComplex]  = input *:* DenseVector.tabulate(N)(i => beta(-i * i)) // pointwise product
//    val coeff: DenseVector[BComplex] = DenseVector.tabulate(N)(i => beta(i * i))
//
//    //    convolved *:* DenseVector.tabulate(N)(k => beta(-k*k))
//  }
//
//  // build radix-r Cooley-Tukey FFT by the "block" and "parallel line" given
//  def cooleyTukeyBuilder[T](
//      input: Seq[T],
//      factors: Seq[Int],
//      block: Seq[T]       => Seq[T],
//      mult: (T, Int, Int) => T,
//      inverse: Boolean = false
//  ): Seq[T] = {
//
//    def twiddle(input: Seq[Seq[T]]) = {
//      val N1 = input.head.size
//      val N2 = input.size
//      input
//        .zip(getIndicesBetween(N1, N2))
//        .map { case (ts, ints) =>
//          ts.zip(ints)
//            .map { case (t, i) => mult(t, if (!inverse) i else -i, N1 * N2) }
//        }
//    }
//
//    def recursiveBuild(input: Seq[T], factors: Seq[Int]): Seq[T] = {
//
//      if (factors.size == 1) block(input)
//      else {
//        val N1 = factors.head
//        val N2 = input.size / N1
//
//        import DSP.interleave.Algos._
//        val input2D = matIntrlv(input, N1, N2).grouped(N1).toSeq // permutation 0
//        //        printlnGreen("after inter0")
//        //        println(input2D.map(_.mkString(" ")).mkString("\n"))
//        val afterBlock = input2D.map(block) // N2 blocks, length = N1
//        //        printlnGreen("after N1 FFT")
//        //        printlnGreen("before twiddle")
//        //        println(afterBlock.map(_.mkString(" ")).mkString("\n"))
//        val afterParallel = twiddle(afterBlock)
//        //        printlnGreen("before inter1")
//        //        println(afterParallel.map(_.mkString(" ")).mkString("\n"))
//        val input2DForRecursion = transpose(afterParallel) // permutation 1(transpose)
//        val afterRecursion      = input2DForRecursion.map(recursiveBuild(_, factors.tail))
//        //        println(afterRecursion.map(_.mkString(" ")).mkString("\n"))
//        val ret = matIntrlv(afterRecursion.flatten, N1, N2) // permutation 2
//        ret
//      }
//    }
//    recursiveBuild(input, factors)
//  }
//
//  /** generate twiddle factor indices in between when decomposed into N1 and N2
//    */
//  def getIndicesBetween(N1: Int, N2: Int) = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1)
//
//  def cooleyTukeyCoeff(N1: Int, N2: Int) = getIndicesBetween(N1, N2).map(_.map(WNnk(N1 * N2, _)))
//
//  def cooleyTukeyFFT(input: Seq[BComplex], factors: Seq[Int]) = {
//    def block(input: Seq[BComplex])               = Refs.FFT(input.toArray).toSeq
//    def mult(input: BComplex, index: Int, N: Int) = input * WNnk(N, index)
//    cooleyTukeyBuilder(input, factors, block, mult)
//  }
//
//  def radixRFFT(input: Seq[BComplex], radix: Int) = {
//    val N = input.size
//    def buildFactors(factors: Seq[Int]): Seq[Int] =
//      if (factors.product == N) factors else buildFactors(factors :+ radix)
//    cooleyTukeyFFT(input, buildFactors(Seq(radix)))
//  }
//
//  def CyclicConv(input: Seq[BComplex], coeff: Seq[BComplex], L: Int): Seq[BComplex] = {
//    require(L >= input.size && L >= coeff.size)
//    // TODO: expand this for different size relationship
//    val paddedInput = input.padTo(L, new BComplex(0, 0))
//    val paddedCoeff = coeff.padTo(L, new BComplex(0, 0)).reverse
//    (1 to L).map { i =>
//      val rotatedCoeff = paddedCoeff.takeRight(i) ++ paddedCoeff.take(L - i) // rotate right
//      paddedInput.zip(rotatedCoeff).map { case (complex, complex1) => complex * complex1 }.reduce(_ + _)
//    }
//  }
//
//  def raderDFT(input: Seq[BComplex]) = {
//    val N = input.size
//    require(SmallPrimes.isPrime(N))
//
//    def getGenerator(modulus: Int) = {
//      def isGenerator(element: Int): Boolean =
//        (1 until modulus).map(i => Zp(modulus).pow(element, i).intValue()).toSet == (1 until modulus).toSet
//      (1 until modulus).dropWhile(!isGenerator(_)).head
//    }
//
//    val g                 = getGenerator(N)
//    val inputPermutation  = (0 until N - 1).map(N - 1 - _).map(i => Zp(N).pow(g, i).intValue())
//    val coeffPermutation  = (0 until N - 1).map(i => Zp(N).pow(g, i).intValue())
//    val outputPermutation = (0 until N - 1).map(i => Zp(N).pow(g, i).intValue())
//
//    val permutedInput                 = inputPermutation.map(input(_))
//    val permutedCoeff                 = coeffPermutation.map(WNnk(N, _) - new BComplex(1, 0))
//    val permutedOutput: Seq[BComplex] = CyclicConv(permutedInput, permutedCoeff, N - 1)
//
//    val output: ArrayBuffer[BComplex] = ArrayBuffer.fill(N)(new BComplex(0, 0)) // output1, output2...output(N-1)
//    output(0) = input.reduce(_ + _)
//    permutedOutput.zip(outputPermutation).foreach { case (complex, i) => output(i) = complex + output(0) }
//
//    output
//  }
//
//  def fold[T](data: Seq[T]) = data.take(data.length / 2).zip(data.takeRight(data.length / 2))
//
//  // transformations
//  def butterflyReal(data: Seq[Double]): Seq[Double] = fold(data).map { case (d, d1) => d + d1 } ++ fold(data).map {
//    case (d, d1) => d - d1
//  }
//  def butterflyComplex(data: Seq[BComplex]): Seq[BComplex] =
//    fold(data).map { case (d, d1) => d + d1 } ++ fold(data).map { case (d, d1) => d - d1 }
//  def swap(data: Seq[Double]) = fold(data).map { case (d, d1) => new BComplex(d, -d1) }
//
//  // reverse transformation
//  def butterflyRealR(data: Seq[Double]): Seq[Double] = fold(data).map { case (d, d1) => (d + d1) } ++ fold(data).map {
//    case (d, d1) => (d - d1)
//  }
//  def butterflyComplexR(data: Seq[BComplex]): Seq[BComplex] =
//    fold(data).map { case (d, d1) => (d + d1) } ++ fold(data).map { case (d, d1) => (d - d1) }
//  def swapR(data: Seq[BComplex]): Seq[Double] = {
//    val reals = data.map(_.real)
//    val imags = data.map(complex => -complex.imag)
//    reals ++ imags
//  }
//
//  def bitReverse(N: Int, data: Int) = BigInt(data.toBinaryString.padToLeft(log2Up(N), '0').reverse, 2).toInt
//
//  def hermitianSymmetricIFFT(input: Seq[BComplex], mode: Int): Seq[BComplex] = {
//    import scala.collection.mutable.Stack
//
//    val N = input.size
//    require(isPow2(N))
//    val stageMax = log2Up(N)
//
//    def fig1R(stack: Stack[BComplex], stage: Int): Seq[BComplex] = {
//      if (stage == stageMax) butterflyComplexR(Seq(stack.pop(), stack.pop()))
//      else {
//        val prev0      = fig1R(stack, stage + 1)
//        val prev1      = fig1R(stack, stage + 1)
//        val indices    = (0 until prev1.length).map(_ * 1 << (stage - 1))
//        val multiplied = prev1.zip(indices).map { case (complex, i) => complex * WNnk(N, -i) }
//        val combined   = prev0 ++ multiplied
//        butterflyComplexR(combined)
//      }
//    }
//
//    def fig2R(stack: Stack[BComplex], stage: Int): Seq[Double] = {
//      if (stage == stageMax) butterflyRealR(Seq(stack.pop(), stack.pop()).map(_.real))
//      else {
//        val prev0      = fig2R(stack, stage + 1)
//        val prev1      = if (stage + 2 <= stageMax) fig1R(stack, stage + 2) else Seq(stack.pop())
//        val indices    = (0 until prev1.length).map(_ * 1 << (stage - 1))
//        val multiplied = prev1.zip(indices).map { case (complex, i) => complex * WNnk(N, -i) }
//        val swapped    = swapR(multiplied)
//        butterflyRealR(prev0 ++ swapped)
//      }
//    }
//
//    val reverseIndices = (0 until N).map(bitReverse(N, _))
//    val droppedIndices = (0 until N)
//      .filter { i =>
//        val up    = 1 << log2Up(i + 1)
//        val level = up / 4
//        level != 0 && (i / level) % 4 == 3
//      }
//      .map(bitReverse(N, _))
//
//    val bitReverseInput = reverseIndices.map(input.apply(_))
//
//    mode match {
//      case 1 => {
//        val dataStack = Stack(bitReverseInput: _*)
//        fig1R(dataStack, 1)
//      }
//      case 2 => {
//        val remainedIndices = reverseIndices.filterNot(droppedIndices.contains(_))
//        val remainedInput   = remainedIndices.map(input.apply(_))
//        val dataStack       = Stack(remainedInput: _*)
//        fig2R(dataStack, 1).map(BComplex(_))
//      }
//    }
//  }
//
//  def r2RealValuedFFT(input: Seq[Double], mode: Int): Seq[BComplex] = {
//    val N = input.size
//    require(isPow2(N))
//    val stageMax = log2Up(N)
//
//    // Fig1, CFFT
//    def fig1(data: Seq[BComplex], stage: Int): Seq[BComplex] = {
//      //      println(s"fig1 stage$stage")
//      if (stage == stageMax) butterflyComplex(data)
//      else {
//        val half                         = data.length / 2
//        val (butterflied0, butterflied1) = butterflyComplex(data).splitAt(half)
//        val indices                      = (0 until half).map(_ * 1 << (stage - 1))
//        val multiplied                   = butterflied1.zip(indices).map { case (complex, i) => complex * WNnk(N, i) }
//        fig1(butterflied0, stage + 1) ++ fig1(multiplied, stage + 1)
//      }
//    }
//
//    // Fig2, afterReduction
//    def fig2(data: Seq[Double], stage: Int): Seq[BComplex] = {
//      //      println(s"fig2 stage$stage")
//      if (stage == stageMax) butterflyReal(data).map(BComplex(_))
//      else {
//        val half                         = data.length / 2
//        val (butterflied0, butterflied1) = butterflyReal(data).splitAt(half)
//        val indices                      = (0 until half).map(_ * 1 << (stage - 1))
//        val multiplied = swap(butterflied1).zip(indices).map { case (complex, i) => complex * WNnk(N, i) }
//        if (multiplied.length > 1) fig2(butterflied0, stage + 1) ++ fig1(multiplied, stage + 2).padTo(half, BComplex(0))
//        else fig2(butterflied0, stage + 1) ++ multiplied.padTo(half, BComplex(0))
//      }
//    }
//
//    val reverseIndices = (0 until N).map(bitReverse(N, _))
//    val droppedIndices = (0 until N)
//      .filter { i =>
//        val up    = 1 << log2Up(i + 1)
//        val level = up / 4
//        level != 0 && (i / level) % 4 == 3
//      }
//      .map(bitReverse(N, _))
//
//    mode match {
//      case 1 => {
//        val bitReversRet = fig1(input.map(BComplex(_)), 1)
//        reverseIndices.map(bitReversRet.apply(_)).take(N / 2)
//      }
//      case 2 => {
//        val bitReversRet = fig2(input, 1)
//        val orderedRet   = reverseIndices.map(bitReversRet.apply(_))
//        (0 until (N / 2)).map(i => if (droppedIndices.contains(i)) orderedRet(N - i).conjugate else orderedRet(i))
//      }
//    }
//  }
//
//}
