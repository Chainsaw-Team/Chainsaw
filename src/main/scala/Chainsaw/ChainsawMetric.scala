package Chainsaw

import Chainsaw.ChainsawMetric.sameMetric

case class ChainsawMetric(
                              elementWise: (BigDecimal, BigDecimal) => Boolean = sameMetric,
                              frameWise: (Seq[BigDecimal], Seq[BigDecimal]) => Boolean
                            )

object ChainsawMetric {
  val sameMetric = (a: BigDecimal, b: BigDecimal) => a == b
}

