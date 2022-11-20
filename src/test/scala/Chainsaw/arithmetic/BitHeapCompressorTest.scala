package Chainsaw.arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._
import Chainsaw.arithmetic.ArithInfoGenerator._
import Chainsaw.xilinx._

import java.io.File
import scala.collection.mutable
import scala.util.Random

class BitHeapCompressorTest extends AnyFlatSpec {

  Random.setSeed(10001)
  def testNaive(): Unit = {
    behavior of "toy compressor tree"

    it should "work at a certain corner case" in {
      val operands =
        Seq( // a testcase including weighted, signed and delayed input
          ArithInfo(64, 0),
          ArithInfo(64, 5),
          ArithInfo(64, 9),
          ArithInfo(64, 12, isPositive = false),
          ArithInfo(64, 15)
        )
      verbose = 1
      val compressorTreeGen = BitHeapCompressor(operands, outputAsCsa = true)
      val data              = Seq.fill(400)(BigInt(64, Random))

      val test = ChainsawTest(
        "testCorner",
        compressorTreeGen,
        data
      )
      test.doTest()
    }
  }

  /** -------- testcases
    * --------
    */
  val testcases: Seq[Seq[(Seq[ArithInfo], InfosShape)]] = Seq(
    RectangularInfos(widthRange = Range.inclusive(100, 200, 100), heightRange = Range.inclusive(50, 100, 50), timeStrategy = IncreaseTimeDiff, upBound = 8),
    RectangularInfos(
      widthRange   = Range.inclusive(50, 100, 50),
      heightRange  = Range.inclusive(50, 75, 25),
      shift        = Random.nextInt(2) + 1,
      withNoise    = true,
      timeStrategy = IncreaseTimeDiff,
      upBound      = 8
    ),
    RectangularInfos(
      widthRange   = Range.inclusive(50, 100, 50),
      heightRange  = Range.inclusive(50, 75, 25),
      shift        = Random.nextInt(2) + 1,
      withNoise    = true,
      mixSign      = true,
      timeStrategy = IncreaseTimeDiff,
      upBound      = 8
    ),
    RectangularInfos(widthRange = Range.inclusive(100, 100), heightRange = Range.inclusive(50, 100, 50)),
    RectangularInfos(widthRange = Range.inclusive(100, 100), heightRange = Range.inclusive(50, 75, 25), shift = Random.nextInt(2) + 1, withNoise = true),
    RectangularInfos(widthRange = Range.inclusive(100, 100), heightRange = Range.inclusive(50, 75, 25), shift = 2, withNoise                     = true, mixSign = true),
    TriangleInfos(widthRange    = Range.inclusive(99, 199, 100)),
    TriangleInfos(
      widthRange         = Range.inclusive(69, 99, 30),
      stairRowShapeRange = Range.inclusive(1, 1),
      stairColShapeRange = Range.inclusive(1, 1),
      truncate           = Range.inclusive(48, 68)
    ),
    TriangleInfos(
      widthRange         = Range.inclusive(69, 99, 30),
      stairRowShapeRange = Range.inclusive(1, 1),
      stairColShapeRange = Range.inclusive(1, 2),
      truncate           = Range.inclusive(5, 68),
      withNoise          = true,
      mixSign            = true
    ),
    TriangleInfos(
      widthRange         = Range.inclusive(69, 99, 30),
      stairRowShapeRange = Range.inclusive(1, 1),
      stairColShapeRange = Range.inclusive(1, 2),
      truncate           = Range.inclusive(5, 30),
      timeStrategy       = IncreaseTimeDiff,
      upBound            = 4
    )
    //    TriangleInfos(
    //      widthRange = Range.inclusive(11, 21, 2),
    //      stairRowShapeRange = Range.inclusive(3, 5),
    //      stairColShapeRange = Range.inclusive(3, 5),
    //      truncate = Range.inclusive(20, 30),
    //      mixSign = true,
    //      timeStrategy = Randomly,
    //      upBound = 10
    //    ),
    //    TriangleInfos(
    //      widthRange = Range.inclusive(11, 21, 2),
    //      stairRowShapeRange = Range.inclusive(3, 5),
    //      stairColShapeRange = Range.inclusive(3, 5),
    //      truncate = Range.inclusive(5, 10),
    //      withNoise = true,
    //      timeStrategy = Randomly,
    //      upBound = 10
    //    )
  )

//  testNaive()

  bitHeapCompressorFuncTest(BasicCompressTree, 100, detail = true)

  //  bitHeapCompressorPerfTest(BasicCompressTree, false)

  def testFuncForInfosOnce(infos: Seq[ArithInfo], target: CompressTreeType, testCount: Int = 1000, detail: Boolean = false) = {
    if (detail) verbose = 1
    val data = (0 until testCount) flatMap (_ => infos.map(info => BigInt(info.width, Random)))
    target match {
      case BasicCompressTree =>
        val compressorGenWithCsa = BitHeapCompressor(infos, outputAsCsa = true)
        val compressorGen        = BitHeapCompressor(infos, outputAsCsa = false)
        val testCsa              = ChainsawTest("testCompressorByCsa", compressorGenWithCsa, data)
        val test                 = ChainsawTest("testCompressor", compressorGen, data)
        testCsa.doTest()
        test.doTest()
    }
  }

  def testPerfForInfosOnce(infos: Seq[ArithInfo], target: CompressTreeType): VivadoReport = {
    target match {
      case BasicCompressTree =>
        val compressorGenWithCsa = BitHeapCompressor(infos, outputAsCsa = true)
        val compressorGen        = BitHeapCompressor(infos, outputAsCsa = false)
        VivadoSynth(compressorGenWithCsa.implH, "synthCompressorWithCsa")
        VivadoSynth(compressorGen.implH, "synthCompressor")
    }
  }

  def bitHeapCompressorFuncTest(target: CompressTreeType, testCount: Int = 1000, detail: Boolean = false): Unit = {
    behavior of s"BitHeap Compressor functional test for ${target.getClass.getSimpleName.init}"
    testcases.flatten.foreach { case (infos, name) => it should s"work correctly on $name" in testFuncForInfosOnce(infos, target, testCount, detail) }
  }

  def bitHeapCompressorPerfTest(target: CompressTreeType, genPerfReportGraph: Boolean = true): Unit = {
    behavior of s"BitHeap Compressor performance test for ${target.getClass.getSimpleName.init}"
    testcases.flatten.foreach { case (infos, shape) =>
      val implReport = testPerfForInfosOnce(infos, target)
      it should s"impl successfully on $shape" in implReport
    }
  }
}
