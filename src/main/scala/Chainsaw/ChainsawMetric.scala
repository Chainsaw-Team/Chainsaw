package Chainsaw

import Chainsaw.ChainsawMetric.sameMetric
import ai.djl.ndarray._
import jdk.jfr.Threshold

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Paths}

case class ChainsawMetric(
    elementWise: (BigDecimal, BigDecimal) => Boolean = sameMetric,
    frameWise: (Seq[BigDecimal], Seq[BigDecimal]) => Boolean
)

object ChainsawMetric {
  val sameMetric = (a: BigDecimal, b: BigDecimal) => a == b

  val sameAsBigInt = (yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =>
    yours.map(_.toBigInt()).equals(golden.map(_.toBigInt()))

  def exportNpz(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    // export data
    val manager = NDManager.newBaseManager()
    val pair =
      new NDList(
        manager.create(yours.map(_.toDouble).toArray),
        manager.create(golden.map(_.toDouble).toArray)
      )
    val os = Files.newOutputStream(Paths.get("pair.npz"))
    pair.encode(os, true)
  }

  def corrMetric(yours: Seq[BigDecimal], golden: Seq[BigDecimal], threshold: Double) = {

    exportNpz(yours, golden)

    val process: Process =
      Runtime.getRuntime.exec(
        "/home/ltr/anaconda3/bin/python /home/ltr/IdeaProjects/Chainsaw/goldenModel/corrMetric.py"
      ); // 执行py文件
    val in   = new BufferedReader(new InputStreamReader(process.getInputStream))
    val line = in.readLine()
    in.close()

    val rets = line.split(" ")
    val ret  = rets(0).toDouble
    val lag  = rets(1).toInt

    logger.info(s"corrcoef = $ret, lag = $lag")

    ret > threshold
  }
}
