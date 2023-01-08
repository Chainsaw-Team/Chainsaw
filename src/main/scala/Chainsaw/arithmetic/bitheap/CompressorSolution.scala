package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic.{CompressorGenerator, bitheap}

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
      case ReFirst => Seq(re, hr, br, rr)
      case HrFirst => Seq(hr, re, br, rr)
    }
    det(priority)
  }

  def <=(that: CompressorScores)(implicit strategy: CompressionStrategy) = !(this > that)

  override def toString = s"CompressorScores(br = $bitReduction, hr = $heightReduction, re = $reductionEfficiency, rr = $reductionRatio)"
}

@SerialVersionUID(42L)
case class CompressorFullSolution(stageSolutions: Seq[CompressorStageSolution]) extends Serializable with HardAlgo {

  def latency = stageSolutions.count(_.pipelined)

  def outHeight = if (stageSolutions.last.stageHeight == -1) 3 else stageSolutions.last.stageHeight

  override def vivadoUtilEstimation = stageSolutions.map(_.vivadoUtilEstimation).reduce(_ + _)

  def bitReduction = stageSolutions.map(_.bitReduction).sum

  def save(file: File): Unit = {
    file.getParentFile.mkdir()
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(this)
    oos.close()
  }

  def report = {
    val classReport = stageSolutions.flatMap(_.compressorSolutions)
      .groupBy(_.compressorName)
      .map { case (k, v) => k -> v.size }.mkString("\n")

    val subReport = stageSolutions.flatMap(_.compressorSolutions)
      .groupBy(_.compressorName)
      .map { case (k, v) => k -> v.map(_.getCompressor().vivadoUtilEstimation).reduce(_ + _) }.mkString("\n")

    val briefReport = s"latency = $latency, reduction = $bitReduction, efficiency = ${bitReduction.toDouble / vivadoUtilEstimation.lut}"

    s"\n----bitheap compressor solution report:----" +
      s"\n--------performance--------" +
      s"\n$briefReport" +
      //      s"\n$classReport" +
      //      s"\n$subReport" +
      s"\n$vivadoUtilEstimation" +
      s"\n--------output status--------" +
      s"\nheight = $outHeight, width = ???"
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

case class CompressorStageSolution(compressorSolutions: Seq[CompressorStepSolution], stageHeight: Int, pipelined: Boolean) {

  def vivadoUtilEstimation = compressorSolutions.map(_.vivadoUtilEstimation).reduce(_ + _)

  def bitReduction = compressorSolutions.map(_.compressorScores.bitReduction).sum

}

case class CompressorStepSolution(compressorName: String, width: Int, columnIndex: Int, compressorScores: CompressorScores = CompressorScores(0, 0, 0, 0)) {
  def getCompressor(complementHeap: Seq[Seq[Boolean]] = null): CompressorGenerator =
    bitheap.getCompressor(compressorName, width, complementHeap)

  def vivadoUtilEstimation = getCompressor().vivadoUtilEstimation
}
