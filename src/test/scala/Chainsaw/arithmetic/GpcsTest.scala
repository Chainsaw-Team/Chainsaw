package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

class GpcsTest extends AnyFlatSpec {

  val testcases = Seq(
    (Compressor4to2, Range.inclusive(Compressor4to2.widthMin, 64)),
    (Compressor3to1, Range.inclusive(Compressor3to1.widthMin, 64)),
    (Compressor6to3, Range.inclusive(Compressor6to3.widthMin, Compressor6to3.widthMax, 4)),
    (Compressor3to2, Range.inclusive(Compressor3to2.widthMin, Compressor3to2.widthMax, 4)),
    (Compressor606To5, Range.inclusive(Compressor606To5.widthMin, Compressor606To5.widthMax, 4))
  )

  behavior of "functional test"

  it should "work correctly on Compressor4to2" in (1 to Compressor4to2.widthMax by 4).foreach(testCompressorFuncOnce(Compressor4to2, _))
  it should "work correctly on Compressor6to3" in (1 to Compressor6to3.widthMax by 4).foreach(testCompressorFuncOnce(Compressor6to3, _))
  it should "work correctly on Compressor3to1" in (1 to Compressor3to1.widthMax by 4).foreach(testCompressorFuncOnce(Compressor3to1, _))
  it should "work correctly on Compressor3to2" in (1 to Compressor3to2.widthMax by 4).foreach(testCompressorFuncOnce(Compressor3to2, _))
  it should "work correctly on Compressor606To5" in (1 to Compressor606To5.widthMax by 4).foreach(testCompressorFuncOnce(Compressor606To5, _))

  behavior of "performance test"

  it should "synth on Compressor4to2 in expected" in (1 to Compressor4to2.widthMax by 4).foreach(testCompressorPerfOnce(Compressor4to2, _))
  it should "synth on Compressor6to3 in expected" in (1 to Compressor6to3.widthMax by 4).foreach(testCompressorPerfOnce(Compressor6to3, _))
  it should "synth on Compressor3to1 in expected" in (1 to Compressor3to1.widthMax by 4).foreach(testCompressorPerfOnce(Compressor3to1, _))
  it should "synth on Compressor3to2 in expected" in (1 to Compressor3to2.widthMax by 4).foreach(testCompressorPerfOnce(Compressor3to2, _))
  it should "synth on Compressor606To5 in expected" in (1 to Compressor606To5.widthMax by 4).foreach(testCompressorPerfOnce(Compressor606To5, _))

  def getModule(compressor: Compressor, width: Int, pipeline: Bool => Bool = bool => bool) = new Module {

    val inputShape  = compressor.inputFormat(width)
    val outputShape = compressor.outputFormat(width)

    val dataIn  = in Bits (inputShape.sum bits)
    val dataOut = out Bits (outputShape.sum bits)

    val bitHeap = ArrayBuffer.fill(inputShape.length)(ArrayBuffer[Bool]())
    val bits    = ArrayBuffer(dataIn.asBools.reverse: _*)
    inputShape.zip(bitHeap).foreach { case (i, container) =>
      val column = bits.take(i)
      container ++= column
      bits --= column
    }
    val ret = compressor.impl(BitHeaps(bitHeap, 0, 0).d(pipeline), width)
    dataOut := ret.d(pipeline).bitHeaps.head.reverse.flatten.asBits()
  }

  case class CompressorPerfReportGraph() {
    val elements                = mutable.Set[(Compressor, Int, VivadoReport)]()
    val fmaxGraphDataInRowAdder = mutable.Set[(String, (String, ArrayBuffer[Int]), (String, ArrayBuffer[Double]))]()
    val areaGraphDataInRowAdder = mutable.Set[(String, (String, ArrayBuffer[Int]), (String, ArrayBuffer[Int]))]()
    val fmaxGraphDataInGPC      = mutable.Set[((String, ArrayBuffer[String]), (String, ArrayBuffer[Double]))]()
    val areaGraphDataInGPC      = mutable.Set[((String, ArrayBuffer[String]), (String, ArrayBuffer[Int]))]()

    def addElement(compressor: Compressor, width: Int, vivadoReport: VivadoReport): elements.type = {
      elements += Tuple3(compressor, width, vivadoReport)
      elements
    }

    private def parseElements(): Unit = {
      val gpcElements      = elements.filter(_._1.isFixed)
      val rowAdderElements = elements.filterNot(_._1.isFixed).groupBy(_._1).toSet

      val compressorNames = ArrayBuffer[String]()
      val fmaxData        = ArrayBuffer[Double]()
      val areaData        = ArrayBuffer[Int]()
      gpcElements.foreach { case (compressor, _, report) =>
        compressorNames += compressor.name
        fmaxData += report.Frequency
        areaData += Seq(report.LUT, report.CARRY8 * 8, (report.FF - compressor.inputBitsCount(-1)).divideAndCeil(2)).max
      }
      fmaxGraphDataInGPC += Tuple2(("row adders", compressorNames), ("fmax", fmaxData))
      areaGraphDataInGPC += Tuple2(("row adders", compressorNames), ("area", areaData))

      rowAdderElements.foreach { case (compressor, infos) =>
        val infoPairs = infos.toSeq.map { case (compressor, w, report) =>
          Tuple3(w, report.Frequency, Seq(report.LUT, report.CARRY8 * 8, (report.FF - compressor.inputBitsCount(w)).divideAndCeil(2)).max)
        }
        val sortedInfoPairs = infoPairs.map(_._1).zip(infoPairs.map(_._2).zip(infoPairs.map(_._3))).sortBy(_._1)
        fmaxGraphDataInRowAdder += Tuple3(compressor.name, ("width", ArrayBuffer(sortedInfoPairs.map(_._1): _*)), ("fmax", ArrayBuffer(sortedInfoPairs.map(_._2._1): _*)))
        areaGraphDataInRowAdder += Tuple3(compressor.name, ("width", ArrayBuffer(sortedInfoPairs.map(_._1): _*)), ("area", ArrayBuffer(sortedInfoPairs.map(_._2._2): _*)))
      }
    }

    // TODO: activate this after matlab environment established

    //    def genPerfReportGraph(showNow: Boolean = false) = {
    //      parseElements
    //      val rowAdderDir = new File(s"$perfReportGraphPath/rowAdder/")
    //      rowAdderDir.mkdirs()
    //      fmaxGraphDataInRowAdder.foreach { case (name, xData, yData) =>
    //        matlabEngine.putVariable("x", xData._2.toArray)
    //        matlabEngine.putVariable("y", yData._2.map(_ / 1e6).toArray)
    //        matlabEngine.eval(s"figure('visible', '${if (showNow) "on" else "off"}')")
    //        matlabEngine.eval("plot(x, y, '-c.', 'LineWidth', 1.5, 'MarkerSize', 15)")
    //        matlabEngine.eval(s"title('the fmax of $name with different width')")
    //        matlabEngine.eval(s"xlabel('${xData._1}')")
    //        matlabEngine.eval(s"ylabel('${yData._1}(MHz)')")
    //        matlabEngine.eval(s"saveas(gcf,'$perfReportGraphPath/rowAdder/fmax_$name.png', 'png')")
    //      }
    //
    //      areaGraphDataInRowAdder.foreach { case (name, xData, yData) =>
    //        matlabEngine.putVariable("x", xData._2.toArray)
    //        matlabEngine.putVariable("y", yData._2.toArray)
    //        matlabEngine.eval(s"figure('visible', '${if (showNow) "on" else "off"}')")
    //        matlabEngine.eval("plot(x, y, '-c.', 'LineWidth', 1.5, 'MarkerSize', 15)")
    //        matlabEngine.eval(s"title('the area cost of $name with different width')")
    //        matlabEngine.eval(s"xlabel('${xData._1}')")
    //        matlabEngine.eval(s"ylabel('${yData._1}')")
    //        matlabEngine.eval(s"saveas(gcf,'$perfReportGraphPath/rowAdder/area_$name.png', 'png')")
    //      }
    //
    //      val gpcDir = new File(s"$perfReportGraphPath/gpc/")
    //      gpcDir.mkdirs()
    //      fmaxGraphDataInGPC.foreach { case (xData, yData) =>
    //        matlabEngine.putVariable("x", Array.tabulate(xData._2.length)(i => i))
    //        matlabEngine.putVariable("y", yData._2.map(_ / 1e6).toArray)
    //        matlabEngine.eval(s"figure('visible', '${if (showNow) "on" else "off"}')")
    //        matlabEngine.eval("plot(x, y, '-c.', 'LineWidth', 1.5, 'MarkerSize', 15)")
    //        matlabEngine.eval(s"title('the fmax of different gpc')")
    //        matlabEngine.eval(s"xlabel('${xData._1}')")
    //        matlabEngine.eval(s"ylabel('${yData._1}(MHz)')")
    //        matlabEngine.eval(s"set(gca, 'XTick', [${Array.tabulate(xData._2.length)(i => i).mkString(" ")}])")
    //        matlabEngine.eval(s"set(gca,'XTickLabel',{${xData._2.map(str => s"'$str'").mkString(",")}})")
    //        matlabEngine.eval(s"saveas(gcf,'$perfReportGraphPath/gpc/${yData._1}.png', 'png')")
    //      }
    //
    //      areaGraphDataInGPC.foreach { case (xData, yData) =>
    //        matlabEngine.putVariable("x", Array.tabulate(xData._2.length)(i => i))
    //        matlabEngine.putVariable("y", yData._2.toArray)
    //        matlabEngine.eval(s"figure('visible', '${if (showNow) "on" else "off"}')")
    //        matlabEngine.eval("plot(x, y, '-c.', 'LineWidth', 1.5, 'MarkerSize', 15)")
    //        matlabEngine.eval(s"title('the area cost of different gpc')")
    //        matlabEngine.eval(s"xlabel('${xData._1}')")
    //        matlabEngine.eval(s"ylabel('${yData._1}')")
    //        matlabEngine.eval(s"set(gca, 'XTick', [${Array.tabulate(xData._2.length)(i => i).mkString(" ")}])")
    //        matlabEngine.eval(s"set(gca,'XTickLabel',{${xData._2.map(str => s"'$str'").mkString(",")}})")
    //        matlabEngine.eval(s"saveas(gcf,'$perfReportGraphPath/gpc/${yData._1}.png', 'png')")
    //      }
    //
    //    }

  }

  def testCompressorFuncOnce(compressor: Compressor, width: Int, testCount: Int = 1000, debug: Boolean = false): Unit = {

    val inputShape  = compressor.inputFormat(width)
    val outputShape = compressor.outputFormat(width)

    def getValueByShape(bigInt: BigInt, shape: Seq[Int]) = {
      val bits = ArrayBuffer(bigInt.toString(2).padToLeft(shape.sum, '0').map(_.asDigit): _*)
      shape.zipWithIndex.map { case (number, weight) =>
        val column = bits.take(number)
        bits --= column
        BigInt(column.sum) << weight
      }.sum
    }

    SimConfig
      .workspaceName(s"${compressor.getClass.getSimpleName.init}_$width")
      .withFstWave
      .compile(getModule(compressor, width))
      .doSim { dut =>
        val data = Seq.fill(testCount)(BigInt(inputShape.sum, Random))
        data.foreach { value =>
          dut.dataIn #= value
          sleep(1)
          val golden = getValueByShape(value, inputShape)
          val yours  = getValueByShape(dut.dataOut.toBigInt, outputShape)
          assert(yours == golden, s"yours: $yours, golden: $golden")
          if (debug) {
            println(s"in : ${value.toString(2)}")
            println(s"out : ${dut.dataOut.toBigInt.toString(2)}")
            println(s"golden : ${golden.toString(2)}")
            println(s"yours : ${yours.toString(2)}")
          }
        }
      }
  }

  def testCompressorPerfOnce(compressor: Compressor, width: Int): VivadoReport = {
    val report = VivadoSynth(getModule(compressor, width, RegNext(_)), name = s"${compressor.getClass.getSimpleName.init}_$width")
    report.require(compressor.utilRequirement(width) + VivadoUtil(lut = 0, ff = compressor.inputFormat(width).sum, dsp = 0, bram36 = 0, uram288 = 0, carry8 = 0), compressor.fMaxRequirement)
    report
  }

  def compressorFuncTest(): Unit = {
    behavior of "functional test"
    testcases.foreach { case (compressor, widths) =>
      it should s"work correctly on ${compressor.name}" in widths.foreach(width => testCompressorFuncOnce(compressor, width))
    }
  }

  def compressorPerfTest(genPerGraph: Boolean = true): Unit = {
    val perfGraph = if (genPerGraph) CompressorPerfReportGraph() else null
    behavior of "performance test"
    testcases.foreach { case (compressor, widths) =>
      widths.foreach { width =>
        val report = testCompressorPerfOnce(compressor, width)
        it should s"synth on ${compressor.name} with width $width in expected" in report
        if (genPerGraph) perfGraph.addElement(compressor, width, report)
      }
    }
    //    perfGraph.genPerfReportGraph()
  }
}
