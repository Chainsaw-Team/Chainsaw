//package Chainsaw.project.zprize
//
//import Chainsaw.{deprecated, _}
//import Chainsaw.arithmetic._
//import Chainsaw.crypto.{BarrettFineAlgo, ModularMultSearch}
//import Chainsaw.deprecated.{Barrett, Bcm, Bm}
//import Chainsaw.project.zprize.ZPrizeMSM.{MPrime, baseModulus}
//import Chainsaw.xilinx.VivadoUtil
//import org.scalatest.flatspec.AnyFlatSpec
//
//import scala.collection.mutable.ArrayBuffer
//import scala.util.Random
//
//class Isca2023Test extends AnyFlatSpec {
//
//  behavior of "regression tests"
//
//  val vu9p = VivadoUtil(lut = 1182000, dsp = 6840)
//  val c1100 = VivadoUtil(lut = 872000, dsp = 5952)
//  val z7020 = VivadoUtil(lut = 85000, dsp = 200)
//
//  /** --------
//   * make sure that the algo works
//   * -------- */
//  val kara192 = BmSolution(BaseDspMult(16, 24), Seq(2, 2), FullMultiplier, Seq(true, true))
//  val schoolbook192 = BmSolution(BaseDspMult(16, 24), Seq(2, 2), FullMultiplier, Seq(false, false))
//  val square192 = BmSolution(BaseDspMult(16, 16), Seq(2, 2, 3), SquareMultiplier, Seq(false, false, false))
//
//  ignore should "work on bm solutions" in {
//    println(kara192)
//    println(schoolbook192)
//    println(square192)
//  }
//
//  def testAlgoFunc(solution: BmSolution): Unit = {
//    val data = solution.multiplierType match {
//      case FullMultiplier => Seq.fill(1000)(BigInt(solution.widthFull, Random), BigInt(solution.widthFull, Random))
//      case SquareMultiplier => Seq.fill(1000) {
//        val x = BigInt(solution.widthFull, Random)
//        (x, x)
//      }
//      case MsbMultiplier => ???
//      case LsbMultiplier => ???
//      case Kara => ???
//    }
//    val algo = BmAlgo(solution)
//    data.foreach { case (x, y) => algo.impl(x, y) }
//  }
//
//  ignore should "work on bm algos" in {
//    testAlgoFunc(kara192)
//    testAlgoFunc(schoolbook192)
//    testAlgoFunc(square192)
//  }
//
//  ignore should "search bm for different widths" in {
//    val fullTimeRecords = ArrayBuffer[Double]()
//    val fullWidthRecords = ArrayBuffer[Int]()
//    (5 until 13).foreach { i =>
//      val width = 1 << i
//      fullWidthRecords += width
//      val startTime = System.nanoTime()
//      MultSearch.getBmParetos(width, FullMultiplier)
//      val endTime = System.nanoTime()
//      fullTimeRecords += (endTime - startTime) / 1e6 // ms
//
//    }
//    println(fullTimeRecords.mkString(","))
//    matlabEngine.putVariable("x", fullWidthRecords.toArray)
//    matlabEngine.putVariable("y", fullTimeRecords.toArray)
//    matlabEngine.eval(s"figure('visible', 'off')")
//    matlabEngine.eval("plot(x, y, '.-', 'LineWidth', 1.5, 'MarkerSize', 15, 'Color', [0.96, 0.64, 0.38])")
//    matlabEngine.eval(s"title('Search time of FullMultiplier')")
//    matlabEngine.eval(s"xlabel('width/bit')")
//    matlabEngine.eval(s"ylabel('time/ms')")
//    matlabEngine.eval(s"saveas(gcf,'src/main/resources/Isca2023/FullMultiplierSearchTime.svg', 'svg')")
//
//    val squareTimeRecords = ArrayBuffer[Double]()
//    val squareWidthRecords = ArrayBuffer[Int]()
//    (5 until 13).foreach { i =>
//      val width = 1 << i
//      squareWidthRecords += width
//      val startTime = System.nanoTime()
//      MultSearch.getBmParetos(width, SquareMultiplier)
//      val endTime = System.nanoTime()
//      squareTimeRecords += (endTime - startTime) / 1e6 // ms
//    }
//    println(squareTimeRecords.mkString(","))
//    matlabEngine.putVariable("x", squareWidthRecords.toArray)
//    matlabEngine.putVariable("y", squareTimeRecords.toArray)
//    matlabEngine.eval(s"figure('visible', 'off')")
//    matlabEngine.eval("plot(x, y, '.-', 'LineWidth', 1.5, 'MarkerSize', 15, 'Color', [0.96, 0.64, 0.38])")
//    matlabEngine.eval(s"title('Search time of SquareMultiplier')")
//    matlabEngine.eval(s"xlabel('width/bit')")
//    matlabEngine.eval(s"ylabel('time/ms')")
//    matlabEngine.eval(s"saveas(gcf,'src/main/resources/Isca2023/SquareMultiplierSearchTime.svg', 'svg')")
//  }
//
//  //  ignore should "search Barrett for different widths" in {
//  //    val timeRecords = ArrayBuffer[Double]()
//  //    val widthRecord = ArrayBuffer[Int]()
//  //    (5 until 13).foreach { i =>
//  //      val width = 1 << i
//  //      widthRecord += width
//  //      val startTime = System.nanoTime()
//  //      val modulus = (BigInt(1) << (width - 1)) + BigInt(width - 1, Random)
//  //      BarrettSearch(width, Some(modulus), FullMultiplier, vu9p)
//  //      val endTime = System.nanoTime()
//  //      timeRecords += (endTime - startTime) / 1e6 // ms
//  //    }
//  //    matlabEngine.putVariable("x", widthRecord.toArray)
//  //    matlabEngine.putVariable("y", timeRecords.toArray)
//  //    matlabEngine.eval(s"figure('visible', 'off')")
//  //    matlabEngine.eval("plot(x, y, '.-', 'LineWidth', 1.5, 'MarkerSize', 15, 'Color', [0.96, 0.64, 0.38])")
//  //    matlabEngine.eval(s"title('Search time of Barrett')")
//  //    matlabEngine.eval(s"xlabel('width/bit')")
//  //    matlabEngine.eval(s"ylabel('time/ms')")
//  //    matlabEngine.eval(s"saveas(gcf,'src/main/resources/Isca2023/BarrettSearchTime.svg', 'svg')")
//  //  }
//
//  /** --------
//   * make sure that the algo is fast enough
//   * -------- */
//  ignore should "work on bm search" in {
//    val vu9p = VivadoUtil(dsp = 6840, lut = 1102400)
//    MultSearch(192, FullMultiplier, vu9p)
//    MultSearch(256, FullMultiplier, vu9p)
//    MultSearch(377, FullMultiplier, vu9p)
//    MultSearch(512, FullMultiplier, vu9p)
//    MultSearch(768, FullMultiplier, vu9p)
//    MultSearch(1024, FullMultiplier, vu9p)
//    MultSearch(2048, FullMultiplier, vu9p)
//    MultSearch(3072, FullMultiplier, vu9p)
//    MultSearch(4096, FullMultiplier, vu9p)
//  }
//
//  /** --------
//   * implementation of variations, table
//   * -------- */
//  // BM variations
//  val width = 377
//  val solution0 = MultSearch.getBmParetos(width, FullMultiplier).maxBy(_.vivadoUtilEstimation.dsp).asInstanceOf[BmSolution]
//  val solution1 = BmSolution(solution0.baseMultiplier, solution0.splits, solution0.multiplierType, true +: Seq.fill(solution0.length - 1)(false))
//  val solution2 = MultSearch.getBmParetos(width, SquareMultiplier).maxBy(_.vivadoUtilEstimation.dsp).asInstanceOf[BmSolution]
//
//  ignore should s"synth for high dsp 377" in {
//    logger.info(solution0.toString)
//    ChainsawSynthOld(Bm(width, None, solution0), s"synthKara$width")
//  }
//
//  ignore should s"synth for schoolbook 377" in {
//    logger.info(solution1.toString)
//    ChainsawSynthOld(deprecated.Bm(width, None, solution1), s"synthKara$width")
//  }
//
//  ignore should s"synth for square 377" in {
//    logger.info(solution2.toString)
//    ChainsawSynthOld(deprecated.Bm(width, None, solution2), s"synthKara$width")
//  }
//
//  // Barrett variations
//  ignore should "synth for variations of Barrett" in {
//    val M = project.zprize.ZPrizeMSM.baseModulus
//    //    ChainsawSynth(Barrett(377, Some(M), FullMultiplier), "testBarrett3770")
//    //    ChainsawSynth(Barrett(377, Some(M), SquareMultiplier), "testBarrett3771")
//    ChainsawSynthOld(Barrett(377, None, FullMultiplier), "testBarrett3772")
//  }
//
//  /** --------
//   * implementation vs model, fig
//   * -------- */
//  testBmImplementation(64)
//  testBmImplementation(128)
//  testBmImplementation(256)
//  testBmImplementation(377)
//  testBmImplementation(512)
//  testBmImplementation(1024)
//
//  def testBmImplementation(width: Int): Unit = {
//    val solution = MultSearch.getBmParetos(width, FullMultiplier).head.asInstanceOf[BmSolution]
//    ignore should s"synth at width $width" in {
//      logger.info(solution.toString)
//      ChainsawSynthOld(deprecated.Bm(width, None, solution), s"synthKara$width")
//    }
//  }
//
//  /** --------
//   * search for different boards, table
//   * -------- */
//
//  ignore should "search for different boards" in {
//    val modulus = project.zprize.ZPrizeMSM.baseModulus
//    //    BarrettSearch(377, Some(modulus), FullMultiplier, vu9p)
//    //    BarrettSearch(377, Some(modulus), FullMultiplier, c1100)
//    ModularMultSearch(377, Some(modulus), FullMultiplier, z7020)
//  }
//
//  ignore should "compare with Kara" in {
//    val solution0 = MultSearch.getBmParetos(68, FullMultiplier).head.asInstanceOf[BmSolution]
//    ChainsawSynthOld(deprecated.Bm(68, None, solution0), "synth68")
//    val solution1 = MultSearch.getBmParetos(102, FullMultiplier).head.asInstanceOf[BmSolution]
//    ChainsawSynthOld(deprecated.Bm(102, None, solution1), "synth102")
//    val solution2 = MultSearch.getBmParetos(119, FullMultiplier).head.asInstanceOf[BmSolution]
//    ChainsawSynthOld(deprecated.Bm(119, None, solution2), "synth119")
//  }
//
//  ignore should "compare with intel" in {
//
//  }
//
//  ignore should "compare with impress" in {
//    val solutions = MultSearch.getBmParetos(1024, FullMultiplier).map(_.asInstanceOf[BmSolution])
//    //    println(BmSearch.getParetos(1024, FullMultiplier).mkString("\n"))
//    solutions.zipWithIndex.foreach { case (solution, i) => ChainsawSynthOld(deprecated.Bm(1024, None, solution), s"synth1024$i") }
//  }
//
//
//  val dataWidth = baseModulus.bitLength
//  val algo = BarrettFineAlgo(baseModulus)
//  val msbWidthInvolved = algo.multMsb.widthInvolved
//  val lsbWidthInvolved = algo.multLsb.widthInvolved
//
//  def msbMultGen = Bcm(MPrime, MsbMultiplier, widthIn = dataWidth + 1, widthInvolved = msbWidthInvolved, widthOut = dataWidth + 1, useCsd = true)
//
//  def lsbMultGen = Bcm(baseModulus, LsbMultiplier, widthIn = dataWidth, widthInvolved = lsbWidthInvolved, widthOut = lsbWidthInvolved, useCsd = true)
//
//  ignore should "work for ZPRIZE MSB mult" in ChainsawSynthOld(msbMultGen, "synth377Msb")
//  ignore should "work for ZPRIZE LSB mult" in ChainsawSynthOld(lsbMultGen, "synth377Lsb")
//}
