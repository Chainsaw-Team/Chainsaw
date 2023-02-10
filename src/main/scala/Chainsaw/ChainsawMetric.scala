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

}
