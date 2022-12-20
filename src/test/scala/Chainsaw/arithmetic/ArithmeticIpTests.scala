package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.arithmetic.ArithInfoGenerator.RectangularInfos
import Chainsaw.project.zprize.ZPrizeMSM

import scala.util.Random

class ArithmeticIpTests extends ChainsawFlatSpec {

  val multTypes = Seq(FullMultiplier, MsbMultiplier, LsbMultiplier, SquareMultiplier)
  val lsbConstants = Seq(Some(ZPrizeMSM.baseModulus), None)
  val msbConstants = Seq(Some(ZPrizeMSM.MPrime), None)

  def forAllBmConfig(func: (MultiplierType, Option[BigInt]) => Unit): Unit = {
    multTypes.foreach { multType =>
      lsbConstants.foreach { constant =>
        if (constant.isEmpty || multType != SquareMultiplier) {
          func(multType, constant)
        }
      }
    }
  }

  def testBmAlgo(): Unit = {
    behavior of "bmAlgo"

    it should "show cost for all configurations" in {
      forAllBmConfig { (multType, constant) =>
        val solution = BmSolution(BaseDspMult(16, 24), splits = Seq(2, 2, 2), multiplierType = multType, isKaras = Seq(true, true, true), constant = constant, threshold = 9)
        BmAlgo(solution)
      }
    }

    forAllBmConfig { (multType, constant) =>
      val solution0 = BmSolution(BaseDspMult(16, 24), splits = Seq(2, 2, 2), multiplierType = multType, isKaras = Seq(true, true, true), constant = constant)
      val solution1 = BmSolution(BaseDspMult(16, 24), splits = Seq(2, 2, 2), multiplierType = multType, isKaras = Seq(false, true, false), constant = constant)
      val solution2 = BmSolution(BaseDspMult(16, 24), splits = Seq(2, 2, 2), multiplierType = multType, isKaras = Seq(false, false, false), constant = constant)
      it should s"work for ${className(multType)} with constant = ${constant}" in {
        BmAlgo(solution0).selfTest()
        BmAlgo(solution1).selfTest()
        BmAlgo(solution2).selfTest()
      }
    }
  }

  def testBm(): Unit = {
    multTypes.foreach { multType =>
      lsbConstants.foreach { constant =>
        if (constant.isEmpty || multType != SquareMultiplier) {
          val solution = BmSolution(
            baseMultiplier = BaseDspMult(16, 24),
            splits = Seq(2, 2, 2),
            multiplierType = multType,
            isKaras = Seq(true, true, true),
            constant = constant)
          val gen = Bm(solution)
          testOperator(gen, generatorConfigTable("Bm"))
        }
      }
    }
  }

  def testBcmAlgo(): Unit = {
    behavior of "BcmAlgo"

    val widthIn = 377
    val constant = lsbConstants.head.get

    val extraWidths = Seq(0, 4, 8)
    val multTypes = Seq(FullMultiplier, MsbMultiplier, LsbMultiplier)
    val useCsds = Seq(true, false)

    multTypes.foreach(multType =>
      useCsds.foreach { useCsd =>
        val extras = if (multType == MsbMultiplier) extraWidths else Seq(0)
        extras.foreach { extraWidth =>
          it should s"work for $widthIn bit ${className(multType)} using ${widthIn + extraWidth} bit and ${if (useCsd) "csd" else "binary"} encoding" in {
            val algo = multType match {
              case FullMultiplier => FullConstantMult(constant = constant, widthIn = widthIn, useCsd = useCsd)
              case MsbMultiplier => MsbConstantMult(constant = constant, widthIn = widthIn, widthInvolved = widthIn + extraWidth, widthOut = widthIn, useCsd = useCsd)
              case LsbMultiplier => LsbConstantMult(constant = constant, widthIn = widthIn, widthOut = widthIn, useCsd = useCsd)
            }
            if (multType == MsbMultiplier) logger.info(s"error bound of msbMult0: ${algo.lowerBound}, ${algo.upperBound}")
            logger.info(s"clb cost of Bcm = ${algo.clbCost}")
            algo.selfTest()
          }
        }
      }
    )
  }

  def testBcm(): Unit = {
    val widthIn = 377
    val constant = lsbConstants.head.get

    val extraWidths = Seq(0, 4, 8)
    //    val multTypes = Seq(FullMultiplier, MsbMultiplier, LsbMultiplier)
    val multTypes: Seq[MultiplierType] = Seq(MsbMultiplier)
    multTypes.foreach { multType =>
      val extras = if (multType == MsbMultiplier) extraWidths else Seq(0)
      extras.foreach { extraWidth =>
        val gen = multType match {
          case FullMultiplier => FullBcm(constant = constant, widthIn = widthIn)
          case MsbMultiplier => MsbBcm(constant = constant, widthIn = widthIn, widthInvolved = widthIn + extraWidth, widthOut = widthIn)
          case LsbMultiplier => LsbBcm(constant = constant, widthIn = widthIn, widthOut = widthIn)
        }
        testOperator(gen, generatorConfigTable("Bcm"))
      }
    }
  }

  def testCompressors() = {
    // row adders
    testOperator(Compressor4to2(8), generatorConfigTable("Compressor"))
    testOperator(Compressor4to2(cpaWidthMax), generatorConfigTable("Compressor"))
    testOperator(Compressor3to1(8), generatorConfigTable("Compressor"))
    testOperator(Compressor3to1(cpaWidthMax), generatorConfigTable("Compressor"))
    // gpcs
    val gpcs = Seq(
      Compressor6to3, Compressor3to2,
      Compressor606, Compressor607, Compressor615, Compressor623,
      Compressor1325, Compressor1415, Compressor1406, Compressor1407, Compressor2117
    )
    gpcs.foreach(testOperator(_, generatorConfigTable("Compressor")))
  }

  def testDspMults(): Unit = {
    val multTypes = Seq(FullMultiplier, MsbMultiplier, LsbMultiplier)
    val smallDivideAndConquers = multTypes.map(Sm)
    val smallTilings = Seq(BaseDspMult(17, 26), BaseDspMult(26, 26), BaseDspMult(34, 34))

    val allMults = smallDivideAndConquers ++ smallTilings
    allMults.foreach(testOperator(_, generatorConfigTable("Dsp")))
  }

  def testMultSearch(): Unit = {
    behavior of "Multiplier search algorithm"

    it should "find paretos for long variable multipliers" in {
      MultSearch.getBmParetos(377, FullMultiplier)
      MultSearch.getBmParetos(377, LsbMultiplier)
      MultSearch.getBmParetos(377, MsbMultiplier)

      MultSearch.getBmParetos(377, FullMultiplier, Some(project.zprize.ZPrizeMSM.baseModulus))
      MultSearch.getBmParetos(377, LsbMultiplier, Some(project.zprize.ZPrizeMSM.baseModulus))
      MultSearch.getBmParetos(377, MsbMultiplier, Some(project.zprize.ZPrizeMSM.baseModulus))
    }
  }

  def testCpa(): Unit = {
    val widths = Seq(63, 127)
    val adderTypes = Seq(BinaryAdder, BinarySubtractor, TernaryAdder, TernarySubtractor1, TernarySubtractor2)
    adderTypes.foreach(adderType =>
      widths.foreach(width =>
        testOperator(Cpa(adderType, width), generatorConfigTable("Cpa"))
      )
    )
  }

  def testBitHeap(): Unit = {
    val infoWithDiffTime = Seq.fill(10)(ArithInfo(width = 10, weight = 0, isPositive = true, time = 0))
    testOperator(BitHeapCompressor(infoWithDiffTime, outputAsCsa = false), generatorConfigTable("BitHeap"))
  }

  def testCsa(): Unit = {
    // TODO: reimplement CSA and its tests
    val infoWithDiffTime = Seq(ArithInfo(10, 0, true, 0), ArithInfo(10, 0, true, 1))
    val gen = Csa(infoWithDiffTime)
    testOperator(gen, generatorConfigTable("Csa"))
  }

  /** --------
   * tests
   * -------- */
  override def algoNames = Seq("BmAlgo", "BcmAlgo", "MultSearch")

  override val generatorConfigTable = Map(
    "Dsp" -> TestConfig(full = true, naive = true, synth = false, impl = false),
    "Compressor" -> TestConfig(full = true, naive = true, synth = false, impl = false),
    "Cpa" -> TestConfig(full = true, naive = true, synth = false, impl = false),
    "BitHeap" -> TestConfig(full = true, naive = true, synth = false, impl = false),
    "Csa" -> TestConfig(full = true, naive = true, synth = false, impl = false),
    "Bcm" -> TestConfig(full = true, naive = true, synth = false, impl = false),
    "Bm" -> TestConfig(full = true, naive = true, synth = false, impl = false),
  )

  //  testDspMults()
  testCpa()
  //  testCompressors()
  //  testBitHeap()
  //  testCsa()
  //  testBmAlgo()
  //  testBm()
  //  testBcmAlgo()
  //  testBcm()
  //  testMultSearch()

}