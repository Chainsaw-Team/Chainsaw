package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.project.zprize.ZPrizeMSM
import org.apache.commons.io.FileUtils

import scala.util.Random

class ArithmeticIpTests extends ChainsawFlatSpec {

  val multTypes: Seq[MultiplierType] =
    Seq(FullMultiplier, MsbMultiplier, LsbMultiplier, SquareMultiplier)
  val adderTypes: Seq[AdderType] = Seq(
    BinaryAdder,
    BinarySubtractor,
    TernaryAdder,
    TernarySubtractor1,
    TernarySubtractor2
  )
  val lsbConstants: Seq[Option[BigInt]] = Seq(Some(ZPrizeMSM.baseModulus), None)
  val msbConstants: Seq[Option[BigInt]] = Seq(Some(ZPrizeMSM.MPrime), None)

  /** traverse all possible combinations of multiplier types for a given
    * bitwidth
    */
  def forAllBmConfig(func: (MultiplierType, Option[BigInt]) => Unit): Unit = {
    multTypes.foreach { multType =>
      lsbConstants.foreach { constant =>
        if (constant.isEmpty || multType != SquareMultiplier) {
          func(multType, constant)
        }
      }
    }
  }

  /** big multiplier by divide-and-conquer, algorithm
    */
  def testBmAlgo(): Unit = {
    behavior of "bmAlgo"

    it should "show cost for all configurations" in {
      forAllBmConfig { (multType, constant) =>
        val solution = BmSolution(
          BaseDspMult(16, 24),
          splits         = Seq(2, 2, 2),
          multiplierType = multType,
          isKaras        = Seq(true, true, true),
          constant       = constant,
          threshold      = 9
        )
        BmAlgo(solution)
      }
    }

    forAllBmConfig { (multType, constant) =>
      // rectangular multiplier + Karatsuba/not Karatusba + split > 2
      val solution = BmSolution(
        BaseDspMult(16, 24),
        splits         = Seq(2, 4),
        multiplierType = multType,
        isKaras        = Seq(true, false),
        constant       = constant
      )
      it should s"work for ${className(multType)} with constant = $constant" in {
        BmAlgo(solution).selfTest()
      }
    }
  }

  /** big multiplier by divide-and-conquer, hardware
    */
  def testBm(): Unit = {

    // small test for quick fail
    val solution =
      MultSearch.getBmParetos(128, FullMultiplier).head.asInstanceOf[BmSolution]
    //    val solution = MultSearch.getBmParetos(377, FullMultiplier).find(_.dspCost == 162).get.asInstanceOf[BmSolution]
    testOperator(Bm(solution), generatorConfigTable("Bm"))

    //    forAllBmConfig { (multType, constant) =>
    //      // rectangular multiplier + Karatsuba/not Karatusba + split > 2
    //      val solution = BmSolution(BaseDspMult(16, 24), splits = Seq(2, 4), multiplierType = multType, isKaras = Seq(true, false), constant = constant)
    //      testOperator(Bm(solution), generatorConfigTable("Bm"))
    //    }
  }

  /** big constant multiplier by bitheap, algorithm
    */
  def testBcmAlgo(): Unit = {
    behavior of "BcmAlgo"

    val widthIn  = 377
    val constant = lsbConstants.head.get

    val extraWidths = Seq(0, 4, 8)
    val multTypes   = Seq(FullMultiplier, MsbMultiplier, LsbMultiplier)
    val useCsds     = Seq(true, false)

    multTypes.foreach(multType =>
      useCsds.foreach { useCsd =>
        val extras = if (multType == MsbMultiplier) extraWidths else Seq(0)
        extras.foreach { extraWidth =>
          it should s"work for $widthIn bit ${className(multType)} using ${widthIn + extraWidth} bit and ${if (useCsd) "csd" else "binary"} encoding" in {
            val algo = multType match {
              case FullMultiplier =>
                FullConstantMult(
                  constant = constant,
                  widthIn  = widthIn,
                  useCsd   = useCsd
                )
              case MsbMultiplier =>
                MsbConstantMult(
                  constant      = constant,
                  widthIn       = widthIn,
                  widthInvolved = widthIn + extraWidth,
                  widthOut      = widthIn,
                  useCsd        = useCsd
                )
              case LsbMultiplier =>
                LsbConstantMult(
                  constant = constant,
                  widthIn  = widthIn,
                  widthOut = widthIn,
                  useCsd   = useCsd
                )
            }
            if (multType == MsbMultiplier)
              logger.info(
                s"error bound of msbMult0: ${algo.lowerBound}, ${algo.upperBound}"
              )
            logger.info(s"clb cost of Bcm = ${algo.clbCost}")
            algo.selfTest()
          }
        }
      }
    )
  }

  /** big constant multiplier by bitheap, hardware
    */
  def testBcm(): Unit = {
    val widthIn  = 377
    val constant = lsbConstants.head.get

    val extraWidths                    = Seq(0, 4, 8)
    val multTypes: Seq[MultiplierType] = Seq(MsbMultiplier)
    multTypes.foreach { multType =>
      val extras = if (multType == MsbMultiplier) extraWidths else Seq(0)
      extras.foreach { extraWidth =>
        val gen = multType match {
          case FullMultiplier => FullBcm(constant = constant, widthIn = widthIn)
          case MsbMultiplier =>
            MsbBcm(
              constant      = constant,
              widthIn       = widthIn,
              widthInvolved = widthIn + extraWidth,
              widthOut      = widthIn
            )
          case LsbMultiplier =>
            LsbBcm(constant = constant, widthIn = widthIn, widthOut = widthIn)
        }
        testOperator(gen, generatorConfigTable("Bcm"))
      }
    }
  }

  def testCompressors(): Unit = {
    Random.setSeed(42)
    println(Compressor4to2(8))
    // row adders
    // TODO: complement heap for 4to2 compressor
    testOperator(Compressor4to2(8), generatorConfigTable("Compressor"))
    testOperator(
      Compressor4to2(cpaWidthMax),
      generatorConfigTable("Compressor")
    )
    // regular compressors
    testOperator(Compressor3to1(8), generatorConfigTable("Compressor"))
    testOperator(
      Compressor3to1(cpaWidthMax - 4),
      generatorConfigTable("Compressor")
    )
    // compressors with complement heap
    testOperator(
      Compressor3to1(
        cpaWidthMax,
        Compressor3to1(cpaWidthMax).getRandomComplementHeap
      ),
      generatorConfigTable("Compressor")
    )
    testOperator(
      Compressor3to1(
        cpaWidthMax,
        Compressor3to1(cpaWidthMax).getRandomComplementHeap
      ),
      generatorConfigTable("Compressor")
    )
    // gpcs
    val gpcs = Seq(
      Compressor6to3(),
      Compressor3to2(),
      // gpcs with complement heap
      Compressor6to3(Compressor6to3().getRandomComplementHeap),
      Compressor3to2(Compressor3to2().getRandomComplementHeap)
    )
    gpcs.foreach(testOperator(_, generatorConfigTable("Compressor")))
  }

  /** test small multipliers implemented by DSPs directly
    */
  def testDspMults(): Unit = {
    val multTypes       = Seq(FullMultiplier, MsbMultiplier, LsbMultiplier)
    val smallKaratsubas = multTypes.map(Sm) // 32 X 32 implemented by Karatsuba
    val smallTilings = Seq(
      BaseDspMult(17, 26),
      BaseDspMult(26, 26),
      BaseDspMult(34, 34),
      BaseDspMult(48, 48)
    )
    //    val allMults = smallKaratsubas ++ smallTilings
    val allMults = smallTilings
    allMults.foreach(testOperator(_, generatorConfigTable("Dsp")))
  }

  def testMultSearch(): Unit = {
    behavior of "Multiplier search algorithm"
    it should "find paretos for long variable multipliers" in {
      forAllBmConfig { (multType, constant) =>
        val paretos = MultSearch.getBmParetos(377, multType, constant)
        // for constant multiplier, a BcmSolution should be contained
        if (constant.isDefined)
          require(paretos.exists(!_.isInstanceOf[BmSolution]))
      }
    }
  }

  def testCpa(): Unit = {
    val widths = Seq(63, 127) // width smaller/larger than cpaWidthMax
    adderTypes.foreach(adderType =>
      widths.foreach(width =>
        testOperator(Cpa(adderType, width), generatorConfigTable("Cpa"))
      )
    )
  }

  def testMerge(): Unit = {

    val weights = Seq((0, 0), (1, 0))
    val signs   = Seq((true, true), (true, false))
    val times   = Seq((0, 0), (0, 1))
    val height  = 5
    // controlled experiments on three features
    //    val widths = Seq(10, 200)
    //    Seq.tabulate(2, 2, 2, 2) { (i, j, k, l) =>
    //      val heap0 = Seq.fill(height)(ArithInfo(widths(l), weights(i)._1, signs(j)._1, times(k)._1))
    //      val heap1 = Seq.fill(height)(ArithInfo(widths(l), weights(i)._2, signs(j)._2, times(k)._2))
    //      testOperator(Merge(heap0 ++ heap1), generatorConfigTable("Merge"))
    //      println(s"width: ${widths(l)}, weight: ${i == 1}, mixed sign: ${j == 1}, different time: ${k == 1}")
    //    }

    // most complicated case
    val heap0 = Seq.fill(height)(ArithInfo(width = 200, weight = 1))
    val heap1 = Seq.fill(height)(
      ArithInfo(width = 200, weight = 0, isPositive = false, time = 1)
    )
    testOperator(Merge(heap0 ++ heap1), generatorConfigTable("Merge"))

    // a merge operation from Karatsuba
    val arithInfosKara = Seq(
      ArithInfo(64, 160),
      ArithInfo(64, 160, isPositive = false),
      ArithInfo(
        64,
        160,
        isPositive = false
      ), // three main parts for cross product
      ArithInfo(32, 192),
      ArithInfo(32, 192),
      ArithInfo(1, 224), // side parts for cross product
      ArithInfo(64, 128),
      ArithInfo(64, 192)
    ) // low and high ter
    testOperator(Merge(arithInfosKara), generatorConfigTable("Merge"))
  }

  /** fast addition algorithm specialized for FPGAs, algorithm
    */
  def testFastAdditionAlgos(): Unit = {
    behavior of "fast addition algos"
    it should "work" in {
      AamAddition(123, 16).selfTest()
      CaiAddition(123, 16).selfTest()
      CcaAddition(123, 16).selfTest()
    }
  }

  /** fast addition algorithm specialized for FPGAs, hardware
    */
  def testCcaAdder(): Unit = {
    val gen = CcaAdder(width = 1024, blockWidth = 32)
    testOperator(gen, generatorConfigTable("CcaAdder"))
  }

  /** -------- tests
    * --------
    */
  override def algoNames =
    Seq("BmAlgo", "BcmAlgo", "MultSearch", "FastAdditionAlgos")

  override val generatorConfigTable = Map(
    "Dsp" -> TestConfig(
      full  = true,
      naive = true,
      synth = true,
      impl  = false,
      // as this is at primitive-level
      utilRequirementStrategy = PreciseRequirement
    ),
    "Compressor" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false,
      // as this is at primitive-level
      utilRequirementStrategy = PreciseRequirement
    ),
    "Cpa" -> TestConfig(full = true, naive = true, synth = false, impl = false),
    "CcaAdder" -> TestConfig(
      full  = true,
      naive = true,
      synth = true,
      impl  = false
    ),
    "Merge" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    ),
    "Bm" -> TestConfig(
      full  = true,
      naive = true,
      synth = true,
      impl  = false
    ),
    "Bcm" -> TestConfig(full = true, naive = true, synth = false, impl = false)
  )

  FileUtils.deleteDirectory(compressorSolutionOutputDir)
  // for reproducibility
  Random.setSeed(42)
  // level 1: compressors and DSPs
  testDspMults()
  testCompressors()
  // level 2: bit (multi-input)adders
  testCpa()
  testFastAdditionAlgos()
  testCcaAdder()
  testMerge()
  // level 3: multipliers
  testBmAlgo()
  testBcmAlgo()
  //  testBm()
  //  testBcm()
  // level 4: DSE
  testMultSearch()
}
