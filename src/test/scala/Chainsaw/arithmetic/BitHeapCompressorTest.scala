package Chainsaw.arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._
import Chainsaw.xilinx._

import java.io.File
import scala.collection.mutable
import scala.util.Random

class BitHeapCompressorTest extends AnyFlatSpec {

  def testNaive(): Unit = {
    behavior of "toy compressor tree"

    it should "work at a certain corner case" in {
      val operands = Seq( // a testcase including weighted, signed and delayed input
        ArithInfo(16, 0),
        ArithInfo(16, 5),
        ArithInfo(16, 9),
        ArithInfo(16, 12, isPositive = false),
        ArithInfo(16, 15)
      )
      verbose = 1
      val compressorTreeGen = BitHeapCompressor(operands)
      val data = Seq.fill(400)(BigInt(16, Random))

      val test = ChainsawTest(
        "testCorner",
        compressorTreeGen,
        data
      )
      test.doTest()
    }
  }

  /** --------
   * testcases
   * -------- */

  val testcases: Seq[Seq[(Seq[ArithInfo], InfosShape)]] = Seq(
    // TODO: test for all cases with timeStrategy = Randomly
    //    RectangularInfos(widthRange = Range.inclusive(100, 200, 100), heightRange = Range.inclusive(10, 100, 20), timeStrategy = Randomly, upBound = 8),
    //    RectangularInfos(
    //      widthRange = Range.inclusive(100, 200, 100),
    //      heightRange = Range.inclusive(10, 100, 20),
    //      shift = Random.nextInt(2) + 1,
    //      withNoise = true,
    //      timeStrategy = Randomly,
    //      upBound = 8
    //    ),
    //    RectangularInfos(
    //      widthRange = Range.inclusive(100, 200, 100),
    //      heightRange = Range.inclusive(10, 100, 20),
    //      shift = Random.nextInt(2) + 1,
    //      withNoise = true,
    //      mixSign = true,
    //      timeStrategy = Randomly,
    //      upBound = 8
    //    ),
    RectangularInfos(widthRange = Range.inclusive(100, 200, 100), heightRange = Range.inclusive(10, 100, 20)),
    RectangularInfos(widthRange = Range.inclusive(100, 200, 100), heightRange = Range.inclusive(10, 100, 20), shift = Random.nextInt(2) + 1, withNoise = true),
    RectangularInfos(widthRange = Range.inclusive(100, 200, 100), heightRange = Range.inclusive(10, 100, 20), shift = 2, withNoise = true, mixSign = true),
    TriangleInfos(widthRange = Range.inclusive(99, 199, 10)),
    TriangleInfos(
      widthRange = Range.inclusive(99, 199, 10),
      stairRowShapeRange = Range.inclusive(1, 5),
      stairColShapeRange = Range.inclusive(1, 5),
      truncate = Range.inclusive(50, 98)
    ),
    TriangleInfos(
      widthRange = Range.inclusive(99, 199, 10),
      stairRowShapeRange = Range.inclusive(2, 5),
      stairColShapeRange = Range.inclusive(2, 5),
      truncate = Range.inclusive(90, 197),
      mixSign = true
    ),
    TriangleInfos(
      widthRange = Range.inclusive(99, 199, 10),
      stairRowShapeRange = Range.inclusive(3, 5),
      stairColShapeRange = Range.inclusive(3, 5),
      truncate = Range.inclusive(180, 296)
    ),
    TriangleInfos(
      widthRange = Range.inclusive(11, 21, 2),
      stairRowShapeRange = Range.inclusive(1, 5),
      stairColShapeRange = Range.inclusive(1, 5),
      truncate = Range.inclusive(5, 10),
      withNoise = true,
      mixSign = true
    ),
    //    TriangleInfos(timeStrategy = Randomly, upBound = 10),
    TriangleInfos(
      widthRange = Range.inclusive(11, 21, 2),
      stairRowShapeRange = Range.inclusive(1, 5),
      stairColShapeRange = Range.inclusive(1, 5),
      truncate = Range.inclusive(5, 10),
      timeStrategy = Increase,
      upBound = 10
    ),
    TriangleInfos(
      widthRange = Range.inclusive(11, 21, 2),
      stairRowShapeRange = Range.inclusive(2, 5),
      stairColShapeRange = Range.inclusive(2, 5),
      truncate = Range.inclusive(10, 20),
      timeStrategy = Decrease,
      upBound = 10
    ),
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

  //    testNaive()

  bitHeapCompressorFuncTest(Basic, 100)
  //
  //  bitHeapCompressorPerfTest(Basic, false)

  sealed trait Strategy

  object Increase extends Strategy

  object Decrease extends Strategy

  object Randomly extends Strategy

  object NoneStrategy extends Strategy

  sealed trait TestTarget

  object Basic extends TestTarget

  /** --------
   * random bit heap generators
   * -------- */
  def genNoise(bound: Int): Int = Random.nextInt(2 * bound + 1) - bound // -8~8

  def genRectangularInfos(width: Int, height: Int, shift: Int = 0, sign: Boolean = true, withNoise: Boolean = false, timeStrategy: Strategy = NoneStrategy, upBound: Int = 0): Seq[ArithInfo] = {
    def randomShift: Int = if (withNoise) Random.nextInt(width) else 0

    val delta = if (height != 1) upBound / (height - 1) else upBound
    Seq.tabulate(height) { i =>
      val noise = randomShift
      timeStrategy match {
        case NoneStrategy => ArithInfo(width - noise, shift + noise, sign)
        case Increase => ArithInfo(width - noise, shift + noise, sign, i * delta)
        case Decrease => ArithInfo(width - noise, shift + noise, sign, (height - i) * delta)
        case Randomly => ArithInfo(width - noise, shift + noise, sign, Random.nextInt(upBound + 1))
      }
    }
  }

  // input bits formed a "triangle", typically generated by a multiplier
  def genTriangleInfos(width: Int, stairShape: (Int, Int) = (1, 1), withNoise: Boolean = false, truncate: Range = null, sign: Boolean = true, timeStrategy: Strategy = NoneStrategy, upBound: Int = 0): Seq[ArithInfo] = {
    require((width / stairShape._1) % 2 == 1, s"the width / stairShape._1 must be odd number ! your input width is $width\t stairShape._1 is ${stairShape._1}")
    if (truncate != null) {
      require(truncate.head >= 0 && truncate.last <= width - 1, s"the truncate range is out of width! your truncate start : ${truncate.head}  end : ${truncate.last}")
    }
    val delta = if (width / stairShape._1 != 1) upBound / (width / stairShape._1 - 1) else upBound
    val infos = (0 until width / stairShape._1).flatMap { i =>
      val mid = ((width / stairShape._1) + 1) / 2
      val shift = (if (withNoise && i > 0) genNoise(stairShape._1 / 5) else if (withNoise && i == 0) genNoise(stairShape._1 / 5).abs else 0) + i * stairShape._1
      val number = mid - (i - mid + 1).abs
      timeStrategy match {
        case NoneStrategy => Seq.fill(number * stairShape._2)(ArithInfo((if (withNoise) genNoise(stairShape._1 / 5) else 0) + stairShape._1, shift, sign))
        case Increase => Seq.fill(number * stairShape._2)(ArithInfo((if (withNoise) genNoise(stairShape._1 / 5) else 0) + stairShape._1, shift, sign, i * delta))
        case Decrease => Seq.fill(number * stairShape._2)(ArithInfo((if (withNoise) genNoise(stairShape._1 / 5) else 0) + stairShape._1, shift, sign, (width / stairShape._1 - i) * delta))
        case Randomly => Seq.fill(number * stairShape._2)(ArithInfo((if (withNoise) genNoise(stairShape._1 / 5) else 0) + stairShape._1, shift, sign, Random.nextInt(upBound + 1)))
      }
    }

    if (truncate != null) infos.filter(arith => arith.high >= truncate.head && arith.low <= truncate.last).map { arith =>
      var newLow = arith.low
      var newHigh = arith.high
      if (arith.low < truncate.head) newLow = truncate.head
      if (arith.high > truncate.last) newHigh = truncate.last
      ArithInfo(newHigh - newLow + 1, newLow, arith.isPositive, arith.time)
    }
    else infos
  }

  /** --------
   * graph generation utils
   * -------- */

  abstract class InfosShape {
    def getConfig: Seq[(String, Any)]
  }

  case class Rectangle(width: Int, height: Int, shift: Int, sign: Boolean, withNoise: Boolean, mixSign: Boolean, timeStrategy: Strategy, upBound: Int) extends InfosShape {
    override def toString = s"Rectangular : width->$width height->$height shift->$shift sign->$sign withNoise->$withNoise mixSign->$mixSign timeStrategy->${timeStrategy.getClass.getSimpleName.init} upBound->$upBound"

    override def getConfig = Seq(("width", width), ("height", height), ("shift", shift), ("sign", sign), ("withNoise", withNoise), ("mixSign", mixSign))
  }

  case class Triangle(width: Int, stairShape: (Int, Int), sign: Boolean, mixSign: Boolean, withNoise: Boolean, truncate: Range, timeStrategy: Strategy, upBound: Int) extends InfosShape {
    override def toString = s"Triangle : width->$width stairShape->$stairShape sign->$sign mixSign->$mixSign withNoise->$withNoise truncate->${if (truncate == null) "All" else s"${truncate.head} to ${truncate.last}"} timeStrategy->${timeStrategy.getClass.getSimpleName.init} upBound->$upBound"

    override def getConfig = Seq(("width", width), ("stairRowShape", stairShape._1), ("stairColShape", stairShape._2), ("withNoise", withNoise), ("truncate", truncate))
  }

  object RectangularInfos {
    def apply(widthRange: Range = Range.inclusive(5, 20, 5), heightRange: Range = Range.inclusive(5, 20, 5), shift: Int = 0, sign: Boolean = true, withNoise: Boolean = false, mixSign: Boolean = false, timeStrategy: Strategy = NoneStrategy, upBound: Int = 0): Seq[(Seq[ArithInfo], InfosShape)] =
      widthRange.flatMap { w =>
        heightRange.map { h =>
          (
            genRectangularInfos(w, h - (if (mixSign) h / 3 else 0), shift, sign, withNoise, timeStrategy, upBound) ++ genRectangularInfos(w, if (mixSign) h / 3 else 0, shift, !sign, withNoise, timeStrategy, upBound),

            Rectangle(width = w, height = h, shift = shift, sign = sign, withNoise = withNoise, mixSign = mixSign, timeStrategy = timeStrategy, upBound = upBound)
          )
        }
      }
  }

  object TriangleInfos {
    def apply(
               widthRange: Range = Range.inclusive(15, 21, 6),
               stairRowShapeRange: Range = Range.inclusive(1, 5, 2),
               stairColShapeRange: Range = Range.inclusive(1, 5, 2),
               withNoise: Boolean = false,
               truncate: Range = null,
               sign: Boolean = true,
               mixSign: Boolean = false,
               timeStrategy: Strategy = NoneStrategy,
               upBound: Int = 0
             ): Seq[(Seq[ArithInfo], InfosShape)] = widthRange.flatMap { w =>
      stairRowShapeRange.flatMap { r =>
        stairColShapeRange.map { c =>
          (
            genTriangleInfos(w * r, (r, c), withNoise, truncate, sign, timeStrategy = timeStrategy, upBound = upBound) ++ (if (mixSign) genTriangleInfos(w * r, (r, c), withNoise, truncate, !sign, timeStrategy = timeStrategy, upBound = upBound) else Seq[ArithInfo]()),
            Triangle(w, (r, c), sign, mixSign, withNoise, truncate, timeStrategy, upBound)
          )
        }
      }
    }
  }

  // graph generator
  case class BitHeapCompressorPerfReportGraph(target: TestTarget) {
    val lutData: mutable.Set[(InfosShape, Int)] = mutable.Set[(InfosShape, Int)]()
    val ffData: mutable.Set[(InfosShape, Int)] = mutable.Set[(InfosShape, Int)]()
    val dspData: mutable.Set[(InfosShape, Int)] = mutable.Set[(InfosShape, Int)]()
    val bramData: mutable.Set[(InfosShape, Int)] = mutable.Set[(InfosShape, Int)]()
    val carry8Data: mutable.Set[(InfosShape, Int)] = mutable.Set[(InfosShape, Int)]()
    val fMaxData: mutable.Set[(InfosShape, Double)] = mutable.Set[(InfosShape, Double)]()

    private def removeFromSeq[A <: Any](seq: Seq[A], index: Int) = {
      val ret = seq.toBuffer
      ret.remove(index)
      ret
    }

    def addPerfData(infoShape: InfosShape, vivadoReport: VivadoReport) = {
      lutData += Tuple2(infoShape, vivadoReport.LUT)
      ffData += Tuple2(infoShape, vivadoReport.FF)
      dspData += Tuple2(infoShape, vivadoReport.DSP)
      bramData += Tuple2(infoShape, vivadoReport.BRAM)
      carry8Data += Tuple2(infoShape, vivadoReport.CARRY8)
      fMaxData += Tuple2(infoShape, vivadoReport.Frequency / 1e6)
    }

    private def getGraphData[V <: AnyVal](rowData: mutable.Set[(InfosShape, V)], resourceType: String) = {
      val differentShapeInfos = rowData
        .map(_._1.getClass)
        .map { dShape => rowData.filter { case (shape, _) => shape.getClass == dShape } }
        .map { dShape => dShape.map { case (shape, value) => (shape.getClass.getSimpleName, shape.getConfig, value) } }

      val configLengths = differentShapeInfos.toSeq.map { dShape => dShape.head._2.length }

      val formatData = configLengths
        .zip(differentShapeInfos)
        .map { case (length, shapeInfo) =>
          Seq.tabulate(length) { l =>
            shapeInfo
              .map { info => removeFromSeq(info._2, l) }
              .map { s => (shapeInfo.filter { i => removeFromSeq(i._2, l) == s }, l) }
          }
        }
        .flatMap(dShape => dShape.map(graphSet => graphSet.filter { case (gData, v) => gData.forall { case (_, config, _) => config(v)._2.isInstanceOf[Int] } }).filter(_.nonEmpty))
        .flatten
      val graphData = formatData.map { case (infos, i) =>
        val shapeName = infos.head._1
        val constantParaStr = s"Constant parameter: ${removeFromSeq(infos.head._2, i).map { case (str, value) => s"$str=$value" }.mkString(";")}"
        val xAxisName = infos.head._2(i)._1
        val yAxisName = resourceType
        val xAxisData = mutable.ArrayBuffer[Any]()
        val yAxisData = mutable.ArrayBuffer[Any]()
        infos.foreach { info =>
          xAxisData += info._2(i)._2
          yAxisData += info._3
        }
        (shapeName, constantParaStr, (xAxisName, xAxisData), (yAxisName, yAxisData))
      }
      graphData
    }

    private def drawGraph(graphData: Seq[(String, String, (String, mutable.ArrayBuffer[Any]), (String, mutable.ArrayBuffer[Any]))], showNow: Boolean): Unit = {
      graphData.foreach { graph =>
        val (xLabel, xData) = graph._3
        val (yLabel, yData) = graph._4
        val title = s"The graph of $yLabel versus $xLabel on ${graph._1} shape using ${target.getClass.getSimpleName.init}"
        val file = new File(defaultCompressorPerfGraphPath, yLabel)
        file.mkdirs()

        val sortedXData = mutable.ArrayBuffer[Double]()
        val sortedYData = mutable.ArrayBuffer[Double]()
        // TODO: activate this after matlab environment established

        //        println(s"before : \n ${xData.mkString(",")} \n ${yData.mkString(",")}")
        //        xData.zip(yData).sortBy(_._1.toString.toDouble).foreach { case (x, y) =>
        //          sortedXData += x.toString.toDouble
        //          sortedYData += y.toString.toDouble
        //        }
        //        println(s"after : \n ${sortedXData.toArray.mkString(",")} \n ${sortedYData.toArray.mkString(",")}")
        //
        //        matlabEngine.putVariable("x", sortedXData.toArray)
        //        matlabEngine.putVariable("y", sortedYData.toArray)
        //        matlabEngine.eval(s"figure('visible', '${if (showNow) "on" else "off"}')")
        //        matlabEngine.eval("plot(x, y, '-c.', 'LineWidth', 1.5, 'MarkerSize', 15)")
        //        matlabEngine.eval(s"title('$title')")
        //        matlabEngine.eval(s"xlabel('$xLabel')")
        //        matlabEngine.eval(s"ylabel('$yLabel')")
        //        matlabEngine.eval(s"legend('${graph._2}', 'location', 'SouthOutside')")
        //        matlabEngine.eval(s"saveas(gcf,'$reportPath/$yLabel/$title.png', 'png')")
        //      }
      }

      def genPerfGraph(showNow: Boolean = false): Unit = {
        val lutGraphData = getGraphData(lutData, resourceType = "LUT")
        val ffGraphData = getGraphData(ffData, resourceType = "FF")
        val dspGraphData = getGraphData(dspData, resourceType = "DSP")
        val bramGraphData = getGraphData(bramData, resourceType = "BRAM")
        val carry8GraphData = getGraphData(carry8Data, resourceType = "CARRY8")
        val fMaxGraphData = getGraphData(fMaxData, resourceType = "FMAX")

        //        drawGraph(lutGraphData, showNow)
        //        drawGraph(ffGraphData, showNow)
        //        drawGraph(dspGraphData, showNow)
        //        drawGraph(bramGraphData, showNow)
        //        drawGraph(carry8GraphData, showNow)
        //        drawGraph(fMaxGraphData, showNow)
      }
    }
  }

  def testFuncForInfosOnce(infos: Seq[ArithInfo], target: TestTarget, testCount: Int = 1000) = {
    val data = (0 until testCount) flatMap (_ => infos.map(info => BigInt(info.width, Random)))
    target match {
      case Basic =>
        val compressorGen = BitHeapCompressor(infos)
        println(s"weight min = ${infos.map(_.weight).min}")
        println(s"weight sum = ${infos.map(_.weight).sum}")
        val test = ChainsawTest("testCompressor", compressorGen, data)
        test.doTest()
    }
  }

  def testPerfForInfosOnce(infos: Seq[ArithInfo], target: TestTarget): VivadoReport = {
    target match {
      case Basic =>
        val compressorGen = BitHeapCompressor(infos)
        VivadoSynth(compressorGen.implH, "synthCompressor")
    }
  }

  def bitHeapCompressorFuncTest(target: TestTarget, testCount: Int = 1000): Unit = {
    behavior of s"BitHeap Compressor functional test for ${target.getClass.getSimpleName.init}"

    testcases.flatten.foreach { case (infos, name) =>
      it should s"work correctly on $name" in testFuncForInfosOnce(infos, target, testCount)
    }
  }

  def bitHeapCompressorPerfTest(target: TestTarget, genPerfReportGraph: Boolean = true): Unit = {
    behavior of s"BitHeap Compressor performance test for ${target.getClass.getSimpleName.init}"
    testcases.flatten.foreach { case (infos, shape) =>
      val implReport = testPerfForInfosOnce(infos, target)
      it should s"impl successfully on $shape" in implReport
    }
  }
}
