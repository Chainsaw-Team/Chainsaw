package Chainsaw.dag

import Chainsaw._
import spinal.core._

abstract class Combinational extends ChainsawGenerator {

  override var latency = 0

  override def implH = null

  def comb(dataIn:Seq[Bits]):Seq[Bits]
}
