package Chainsaw.arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._
import Chainsaw.arithmetic.ArithInfoGenerator._
import Chainsaw.xilinx._

import java.io.File
import scala.collection.mutable
import scala.util.Random
import testConfigurations._

class BitHeapCompressorTest extends ChainsawFlatSpec {

  val asCsas = Seq(true, false)
  testcases.map(_._1).distinct.foreach(infos =>
    asCsas.foreach(asCsa =>
      testGenerator(BitHeapCompressor(infos, asCsa), bitHeapCompressorSynth, bitHeapCompressorImpl)
    ))

  def testFuncForInfosOnce(infos: Seq[ArithInfo], target: CompressTreeType): Unit = {
    target match {
      case BasicCompressTree =>
        val compressorGenWithCsa = BitHeapCompressor(infos, outputAsCsa = true)
        val compressorGen = BitHeapCompressor(infos, outputAsCsa = false)
        testGenerator(compressorGenWithCsa)
        testGenerator(compressorGen)
    }
  }

  def testcases: Seq[(Seq[ArithInfo], InfosShape)] = Seq(
    RectangularInfos(
      widthRange = Range.inclusive(100, 200, 100),
      heightRange = Range.inclusive(50, 100, 50),
      timeStrategy = IncreaseTimeDiff,
      upBound = 8),
    RectangularInfos(
      widthRange = Range.inclusive(50, 100, 50),
      heightRange = Range.inclusive(50, 75, 25),
      shift = Random.nextInt(2) + 1,
      withNoise = true,
      timeStrategy = IncreaseTimeDiff,
      upBound = 8
    ),
    RectangularInfos(
      widthRange = Range.inclusive(50, 100, 50),
      heightRange = Range.inclusive(50, 75, 25),
      shift = Random.nextInt(2) + 1,
      withNoise = true,
      mixSign = true,
      timeStrategy = IncreaseTimeDiff,
      upBound = 8
    ),
    RectangularInfos(
      widthRange = Range.inclusive(100, 100),
      heightRange = Range.inclusive(50, 100, 50)),
    RectangularInfos(
      widthRange = Range.inclusive(100, 100),
      heightRange = Range.inclusive(50, 75, 25),
      shift = Random.nextInt(2) + 1,
      withNoise = true),
    RectangularInfos(
      widthRange = Range.inclusive(100, 100),
      heightRange = Range.inclusive(50, 75, 25),
      shift = 2,
      withNoise = true,
      mixSign = true),
    // FIXME: TriangularInfos will lead to stackOverFlow while inferring widths

    //    TriangleInfos(
    //      widthRange = Range.inclusive(99, 199, 100)),
    //    TriangleInfos(
    //      widthRange = Range.inclusive(69, 99, 30),
    //      stairRowShapeRange = Range.inclusive(1, 1),
    //      stairColShapeRange = Range.inclusive(1, 1),
    //      truncate = Range.inclusive(48, 68)
    //    ),
    //    TriangleInfos(
    //      widthRange = Range.inclusive(69, 99, 30),
    //      stairRowShapeRange = Range.inclusive(1, 1),
    //      stairColShapeRange = Range.inclusive(1, 2),
    //      truncate = Range.inclusive(5, 68),
    //      withNoise = true,
    //      mixSign = true
    //    ),
    //    TriangleInfos(
    //      widthRange = Range.inclusive(69, 99, 30),
    //      stairRowShapeRange = Range.inclusive(1, 1),
    //      stairColShapeRange = Range.inclusive(1, 2),
    //      truncate = Range.inclusive(5, 30),
    //      timeStrategy = IncreaseTimeDiff,
    //      upBound = 4
    //    )
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
  ).flatten

}
