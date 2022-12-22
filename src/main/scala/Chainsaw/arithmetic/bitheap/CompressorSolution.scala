package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._

import java.io._

case class CompressorScores(bitReduction: Int, heightReduction: Int, reductionEfficiency: Double, reductionRatio: Double) {

  /** compare two compressor according to current strategy
   */
  def >(that: CompressorScores)(implicit strategy: CompressionStrategy): Boolean = {

    val br = this.bitReduction compare that.bitReduction
    val hr = this.heightReduction compare that.heightReduction
    val re = this.reductionEfficiency compare that.reductionEfficiency
    val rr = this.reductionRatio compare that.reductionRatio

    def det(compared: Seq[Int]) = compared.reverse.zipWithIndex.map { case (bit, weight) => bit << weight }.sum > 0

    val priority = strategy match {
      case EfficiencyFirst => Seq(re, hr, br, rr)
      case ReductionFirst => Seq(hr, re, br, rr)
    }
    det(priority)
  }

  def <=(that: CompressorScores)(implicit strategy: CompressionStrategy) = !(this > that)

  override def toString = s"CompressorScores(br = $bitReduction, hr = $heightReduction, re = $reductionEfficiency, rr = $reductionRatio)"
}

@SerialVersionUID(42L)
case class CompressorFullSolution(stageSolutions: Seq[CompressorStageSolution])
  extends Serializable with HardAlgo {

  def latency() = stageSolutions.count(_.pipelined)

  override def vivadoUtilEstimation = stageSolutions.map(_.vivadoUtilEstimation).reduce(_ + _)

  def save(file: File): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(this)
    oos.close()
  }
}

object CompressorFullSolution {
  def load(file: java.io.File) = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    val ret = ois.readObject().asInstanceOf[CompressorFullSolution]
    ois.close()
    ret
  }
}

case class CompressorStageSolution(compressorSolutions: Seq[CompressorStepSolution], pipelined: Boolean) {

  def vivadoUtilEstimation = compressorSolutions.map(_.vivadoUtilEstimation).reduce(_ + _)

}

case class CompressorStepSolution(compressorName: String, width: Int, columnIndex: Int, compressorScores: CompressorScores) {
  def getCompressor = bitheap.getCompressor(compressorName, width)

  def vivadoUtilEstimation = getCompressor.vivadoUtilEstimation
}