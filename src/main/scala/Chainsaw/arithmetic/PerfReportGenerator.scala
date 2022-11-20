package Chainsaw.arithmetic

import Chainsaw.CompressTreeType
import Chainsaw.arithmetic.ArithInfoGenerator.InfosShape
import Chainsaw.xilinx.VivadoReport

import java.io.File
import scala.collection.mutable

// graph generator
case class PerfReportGenerator(target: CompressTreeType) {
  val lutData: mutable.Set[(InfosShape, Int)]     = mutable.Set[(InfosShape, Int)]()
  val ffData: mutable.Set[(InfosShape, Int)]      = mutable.Set[(InfosShape, Int)]()
  val dspData: mutable.Set[(InfosShape, Int)]     = mutable.Set[(InfosShape, Int)]()
  val bramData: mutable.Set[(InfosShape, Int)]    = mutable.Set[(InfosShape, Int)]()
  val carry8Data: mutable.Set[(InfosShape, Int)]  = mutable.Set[(InfosShape, Int)]()
  val fMaxData: mutable.Set[(InfosShape, Double)] = mutable.Set[(InfosShape, Double)]()

  private def removeFromSeq[A <: Any](seq: Seq[A], index: Int) = {
    val ret = seq.toBuffer
    ret.remove(index)
    ret
  }

  def addPerfData(infoShape: InfosShape, vivadoReport: VivadoReport): Unit = {
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
      val shapeName       = infos.head._1
      val constantParaStr = s"Constant parameter: ${removeFromSeq(infos.head._2, i).map { case (str, value) => s"$str=$value" }.mkString(";")}"
      val xAxisName       = infos.head._2(i)._1
      val yAxisName       = resourceType
      val xAxisData       = mutable.ArrayBuffer[Any]()
      val yAxisData       = mutable.ArrayBuffer[Any]()
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
      val title           = s"The graph of $yLabel versus $xLabel on ${graph._1} shape using ${target.getClass.getSimpleName.init}"
      val file            = new File(defaultCompressorPerfGraphPath, yLabel)
      file.mkdirs()

      val sortedXData = mutable.ArrayBuffer[Double]()
      val sortedYData = mutable.ArrayBuffer[Double]()
      // TODO: activate this after matlab environment established

      xData.zip(yData).sortBy(_._1.toString.toDouble).foreach { case (x, y) =>
        sortedXData += x.toString.toDouble
        sortedYData += y.toString.toDouble
      }

      //      matlabEngine.putVariable("x", sortedXData.toArray)
      //      matlabEngine.putVariable("y", sortedYData.toArray)
      //      matlabEngine.eval(s"figure('visible', '${if (showNow) "on" else "off"}')")
      //      matlabEngine.eval("plot(x, y, '-c.', 'LineWidth', 1.5, 'MarkerSize', 15)")
      //      matlabEngine.eval(s"title('$title')")
      //      matlabEngine.eval(s"xlabel('$xLabel')")
      //      matlabEngine.eval(s"ylabel('$yLabel')")
      //      matlabEngine.eval(s"legend('${graph._2}', 'location', 'SouthOutside')")
      //      matlabEngine.eval(s"saveas(gcf,'$reportPath/$yLabel/$title.png', 'png')")
    }
  }

  def genPerfGraph(showNow: Boolean = false): Unit = {
    val lutGraphData    = getGraphData(lutData, resourceType = "LUT")
    val ffGraphData     = getGraphData(ffData, resourceType = "FF")
    val dspGraphData    = getGraphData(dspData, resourceType = "DSP")
    val bramGraphData   = getGraphData(bramData, resourceType = "BRAM")
    val carry8GraphData = getGraphData(carry8Data, resourceType = "CARRY8")
    val fMaxGraphData   = getGraphData(fMaxData, resourceType = "FMAX")

    drawGraph(lutGraphData, showNow)
    drawGraph(ffGraphData, showNow)
    drawGraph(dspGraphData, showNow)
    drawGraph(bramGraphData, showNow)
    drawGraph(carry8GraphData, showNow)
    drawGraph(fMaxGraphData, showNow)
  }
}

