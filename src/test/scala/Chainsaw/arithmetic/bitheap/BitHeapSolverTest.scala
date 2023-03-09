package Chainsaw.arithmetic.bitheap

import Chainsaw.arithmetic._
import Chainsaw._
import org.apache.commons.io.FileUtils
import ArithInfoGenerator._

import java.io._

class BitHeapSolverTest extends ChainsawFlatSpec {

  /** --------
    *   1. testCases
    * --------
    */
  val testCases: Seq[Seq[ArithInfo]] = Seq(
    //    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, time = 1)), // positive, diff time
    //    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, isPositive = false)), // mixed, same time
    //    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, isPositive = false, time = 1)), // mixed, same time
    //    Seq.fill(5)(ArithInfo(100, 0)) ++ Seq.fill(5)(ArithInfo(100, 0, isPositive = false, time = 1)), // mixed, diff time
    //    Seq.fill(5)(ArithInfo(10, 2)) ++ Seq.fill(5)(ArithInfo(10, 1, isPositive = false, time = 1)), // mixed, diff time

    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60
    ), // rectangle, positive, same time
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      withNoise = true
    ), // rectangle, positive, same time, withNoise
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      withNoise    = true,
      timeStrategy = IncreaseTimeDiff,
      timeUpBound  = 10
    ), // rectangle, positive, diff time(Increase), withNoise
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      withNoise    = true,
      timeStrategy = RandomTimeDiff,
      timeUpBound  = 10
    ), // rectangle, positive, diff time(Random), withNoise
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      timeStrategy = IncreaseTimeDiff,
      timeUpBound  = 10
    ), // rectangle, positive, diff time(Increase)
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      timeStrategy = RandomTimeDiff,
      timeUpBound  = 10
    ), // rectangle, positive, diff time(Random)
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      randomSign = true
    ), // rectangle, mixSign, same time
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      withNoise  = true,
      randomSign = true,
      shift      = 2
    ), // rectangle, mixSign, same time, withNoise
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      randomSign   = true,
      shift        = 2,
      timeStrategy = RandomTimeDiff,
      timeUpBound  = 10
    ), // rectangle, mixSign, diff time
    RectangularInfos(
      50 to 100 by 60,
      50 to 100 by 60,
      withNoise    = true,
      randomSign   = true,
      shift        = 2,
      timeStrategy = RandomTimeDiff,
      timeUpBound  = 10
    ), // rectangle, mixSign, diff time, withNoise

    TriangleInfos(widthRange = 50 to 100 by 60),                    // triangle, positive, same time
    TriangleInfos(widthRange = 100 to 100 by 50, withNoise = true), // triangle, positive, same time, withNoise
    TriangleInfos(
      widthRange   = 100 to 100 by 50,
      timeStrategy = RandomTimeDiff,
      timeUpBound  = 10
    ), // triangle, positive, diff time(Random)
    TriangleInfos(
      widthRange   = 100 to 100 by 50,
      timeStrategy = RandomTimeDiff,
      timeUpBound  = 10,
      withNoise    = true
    ),                                                               // triangle, positive, diff time(Random), withNoise
    TriangleInfos(widthRange = 100 to 100 by 50, randomSign = true), // triangle, mixedSign, same time
    TriangleInfos(
      widthRange = 100 to 100 by 50,
      randomSign = true,
      withNoise  = true
    ), // triangle, mixedSign, same time, withNoise
    TriangleInfos(
      widthRange   = 100 to 100 by 50,
      randomSign   = true,
      timeStrategy = RandomTimeDiff,
      timeUpBound  = 10
    ), // triangle, mixedSign, diff time
    TriangleInfos(
      widthRange   = 100 to 100 by 50,
      randomSign   = true,
      withNoise    = true,
      timeStrategy = RandomTimeDiff,
      timeUpBound  = 10
    ) // triangle, mixedSign, diff time, withNoise
  ).flatten.map(_._1)

  /** -------- 2. solver tester function
    * --------
    */
  def testSolver(solverUnderTest: BitHeapSolver): Unit = {
    FileUtils.deleteDirectory(compressorSolutionOutputDir)
    compressorSolutionOutputDir.mkdir()
    testCases.foreach(testCase => {
      val gen = BitHeapCompressor(testCase, solverUnderTest)
      testOperator(gen, generatorConfigTable("BitHeapCompressor"))
    })
  }

  def testNaiveSolver(): Unit = testSolver(NaiveSolver)

  def testGreedSolver(): Unit = testSolver(GreedSolver)

  def testStrategySeparationSolver(): Unit = testSolver(
    StrategySeparationSolver
  )

  def testStrategySeparationOptimizedSolver(): Unit = testSolver(
    StrategySeparationOptimizedSolver
  )

  def testTernaryTreeSolver(): Unit = testSolver(TernaryTreeSolver)

  def testGpcSolver(): Unit = testSolver(GpcSolver)

  def testBestSolver(): Unit = testSolver(solverUnderTest = null)

  def evalHeapSize(
      widthRange: Range,
      heightRange: Range    = Range(10, 20, 10),
      isRectangle: Boolean  = true,
      solver: BitHeapSolver = null
  ) = {
    val evalResultFile = new File(
      s"src/main/resources/evalSizeResults",
      s"${hashName(s"Width[${widthRange.start}:${widthRange.step}:${widthRange.end}]_Height[${heightRange.start}:${heightRange.step}:${heightRange.end}]_${if (isRectangle) "Rectangle" else "Triangle"}_${if (solver == null) "InferredSolver"
      else s"${solver.solverName}"}")}.txt"
    )
    var resultString = ""
    if (!evalResultFile.exists()) {
      val testCases = widthRange
        .flatMap { w =>
          heightRange.map { h =>
            (w, h) -> BitHeapGroup.fromInfos(
              if (isRectangle) genRectangularInfos(w, h)
              else genTriangleInfos(w)
            )
          }
        }
      val solutions =
        testCases.map(testCase =>
          testCase._1 -> (if (solver == null)
                            searchBestSolver(testCase._2).scores
                          else solver.solveAll(testCase._2).scores)
        )

      // eval width influence
      val sortedSolutionByWidth =
        solutions
          .groupBy(_._1._2)
          .toSeq
          .sortBy(_._1)
          .map { case (h, scoresWithSize) =>
            h -> scoresWithSize.map { case (size, scores) => (size._1, scores) }
          }
      // eval height influence
      val sortedSolutionByHeight =
        solutions
          .groupBy(_._1._1)
          .toSeq
          .sortBy(_._1)
          .map { case (h, scoresWithSize) =>
            h -> scoresWithSize.map { case (size, scores) => (size._2, scores) }
          }

      def padTarget(target: String) = target match {
        case "br" => "bit reduction"
        case "re" => "reduction efficiency"
        case "rr" => "reduction ratio"
        case "hr" => "height reduction"
        case _    => target
      }
      def targetSeparator(target: String) =
        s"\n****the comparison of ${padTarget(target)}****\n\n"
      def targetResultByWidth(target: String) = {
        sortedSolutionByWidth
          .map { case (h, scoresWithWidth) =>
            h -> scoresWithWidth
              .map { case (w, scores) =>
                w -> scores.getOrElse(target, 0.0)
              }
              .sortBy(_._2)
          }
          .zipWithIndex
          .map { case ((h, scoresWithWidth), i) =>
            val sortedWidths = scoresWithWidth.map(_._1).reverse
            s"${i + 1}.height = $h, the comparison of ${padTarget(target)}:\n" +
              s"${sortedWidths.mkString(">")}\n" +
              s"best width is ${sortedWidths.head}\n" +
              s"${if (sortedWidths.head == widthRange.max) "maybe not reach peak, can add widthMax to test"
              else s"get a peak in ${sortedWidths.head}, where widthMax is ${widthRange.max}"}\n"
          }
          .mkString("\n")
      }

      def targetResultByHeight(target: String) = {
        sortedSolutionByHeight
          .map { case (w, scoresWithHeight) =>
            w -> scoresWithHeight
              .map { case (h, scores) =>
                h -> scores.getOrElse(target, 0.0)
              }
              .sortBy(_._2)
          }
          .zipWithIndex
          .map { case ((w, scoresWithHeight), i) =>
            val sortedHeights = scoresWithHeight.map(_._1).reverse
            s"${i + 1}.width = $w, the comparison of ${padTarget(target)}:\n" +
              s"${sortedHeights.mkString(">")}\n" +
              s"best height is ${sortedHeights.head}\n" +
              s"${if (sortedHeights.head == heightRange.max) "maybe not reach peak, can add heightMax to test"
              else s"get a peak in ${sortedHeights.head}, where heightMax is ${heightRange.max}"}\n"
          }
          .mkString("\n")
      }
      val header = s"Evaluate the effect of BitHeap size on compress scores\n\n"
      val evalWidthStart =
        s"--------the effect of BitHeap width on scores--------\n"
      val evalHeightStart =
        s"--------the effect of BitHeap Height on scores--------\n"

      resultString = s"$header$evalWidthStart${Seq("br", "cost", "re", "rr", "hr")
        .map(target => s"${targetSeparator(target)}${targetResultByWidth(target)}")
        .mkString("")}\n$evalHeightStart${Seq("br", "cost", "re", "rr", "hr")
        .map(target => s"${targetSeparator(target)}${targetResultByHeight(target)}")
        .mkString("")}"

      FileUtils.writeStringToFile(evalResultFile, resultString)
    } else {
      resultString = FileUtils.readFileToString(evalResultFile)
    }

    logger.info(resultString)

  }
  override def generatorConfigTable = Map(
    "BitHeapCompressor" -> TestConfig(
      full  = true,
      naive = false,
      synth = false,
      impl  = false
    )
  )

//  testNaiveSolver()
//  testGreedSolver()
//  testStrategySeparationSolver()
//  testStrategySeparationOptimizedSolver()
//  testTernaryTreeSolver()
//  testGpcSolver()
  testBestSolver()
//  evalHeapSize(Range.inclusive(190, 300, 10), Range.inclusive(80, 100, 20), true)
}
