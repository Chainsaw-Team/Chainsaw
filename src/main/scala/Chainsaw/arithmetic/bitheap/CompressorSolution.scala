package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic.{CompressorGenerator, bitheap}

import java.io._

case class ScoreIndicator(
    bitReduction: Int,
    heightReduction: Int,
    reductionEfficiency: Double,
    reductionRatio: Double,
    cost: Double
) {

  /** compare two compressor according to current strategy
    */
  def >(
      that: ScoreIndicator
  )(implicit strategy: CompressionStrategy): Boolean = {

    val br = this.bitReduction compare that.bitReduction
    val hr = this.heightReduction compare that.heightReduction
    val re = this.reductionEfficiency compare that.reductionEfficiency
    val rr = this.reductionRatio compare that.reductionRatio
    val co = this.cost compare that.cost

    def det(compared: Seq[Int]) = compared.reverse.zipWithIndex.map { case (bit, weight) =>
      bit << weight
    }.sum > 0

    val priority = strategy match {
      case ReFirst => Seq(re, hr, br, rr, co)
      case HrFirst => Seq(hr, re, br, rr, co)
    }
    det(priority)
  }

  def <=(that: ScoreIndicator)(implicit strategy: CompressionStrategy) =
    !(this > that)

  def toMap = Map(
    "br"   -> bitReduction.toDouble,
    "cost" -> cost,
    "re"   -> reductionEfficiency,
    "rr"   -> reductionRatio,
    "hr"   -> heightReduction.toDouble
  )

  override def toString =
    s"ScoresIndicator(br = $bitReduction, hr = $heightReduction, re = $reductionEfficiency, rr = $reductionRatio)"
}

@SerialVersionUID(42L)
case class CompressorFullSolution(stageSolutions: Seq[CompressorStageSolution]) extends Serializable with HardAlgo {

  def latency = stageSolutions.count(_.pipelined)

  def outHeight = if (stageSolutions.last.stageOutHeight == -1) 3
  else stageSolutions.last.stageOutHeight

  override def vivadoUtilEstimation =
    stageSolutions.map(_.vivadoUtilEstimation).reduce(_ + _)

  def bitReduction = stageSolutions.map(_.bitReduction).sum

  def save(file: File): Unit = {
    file.getParentFile.mkdir()
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(this)
    oos.close()
  }

  def scores: Map[String, Double] = {
    val scoreSum = stageSolutions
      .flatMap(_.scores)
      .groupBy(_._1)
      .map { case (k, v) =>
        k match {
          case "rr" => k -> v.map(_._2).sum / v.length
          case _    => k -> v.map(_._2).sum
        }
      }
    scoreSum.map { case (name, score) =>
      name match {
        case "re" =>
          name -> scoreSum.getOrElse("br", 0.0) / scoreSum.getOrElse(
            "cost",
            0.0
          )
        case _ => name -> score
      }
    }
  }

  def toTable(
      rows: Seq[Map[String, Double]],
      tail: Map[String, Double] = null
  ) = {
    val scoreOrder = Seq("br", "cost", "re", "rr", "hr")
    val sortedRows =
      if (tail == null)
        rows.map(row => scoreOrder.map(k => row.getOrElse(k, 0.0)))
      else
        (rows :+ tail).map(row => scoreOrder.map(k => row.getOrElse(k, 0.0)))

    val colWidths = sortedRows.transpose
      .map(col => col.map(_.toString.length).max)
    val header =
      ("Stage" +: scoreOrder.zip(colWidths).map { case (str, len) =>
        s"${" " * ((len - str.length) / 2)}$str${" " * ((len + 1 - str.length) / 2)}"
      }).mkString("| ", " | ", " |") + s"\n"
    val separator = (5 +: colWidths)
      .map(len => s"${"-" * len}")
      .mkString("| ", " | ", " |") + s"\n"
    val body = sortedRows.zipWithIndex
      .map { case (scores, stage) =>
        (s"${(if (stage + 1 == sortedRows.length && tail != null) "total"
              else (stage + 1).toString).padTo(5, " ").mkString("")}" +:
          scores
            .zip(colWidths)
            .map { case (score, len) =>
              val scoreLength = score.toString.length
              s"${" " * ((len - scoreLength) / 2)}${score.toString}${" " * ((len - scoreLength + 1) / 2)}"
            })
          .mkString("| ", " | ", " |")
      }
      .mkString("\n")
    s"$header$separator$body"
  }

  override def toString = {
    val rows = stageSolutions.map(_.scores)
    val tail = scores
    toTable(rows, tail)
  }

  def report = {
    val classReport = stageSolutions
      .flatMap(_.compressorSolutions)
      .groupBy(_.compressorName)
      .map { case (k, v) => k -> v.size }
      .mkString("\n")

    val subReport = stageSolutions
      .flatMap(_.compressorSolutions)
      .groupBy(_.compressorName)
      .map { case (k, v) =>
        k -> v.map(_.getCompressor().vivadoUtilEstimation).reduce(_ + _)
      }
      .mkString("\n")

    val briefReport =
      s"latency = $latency, reduction = $bitReduction, efficiency = ${bitReduction.toDouble / vivadoUtilEstimation.lut}"

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

case class CompressorStageSolution(
    compressorSolutions: Seq[CompressorStepSolution],
    stageInHeight: Int,
    stageOutHeight: Int,
    pipelined: Boolean,
    stageIndex: Int
) {

  def vivadoUtilEstimation =
    compressorSolutions.map(_.vivadoUtilEstimation).reduce(_ + _)

  def bitReduction =
    compressorSolutions.map(_.compressorScores.bitReduction).sum

  def scores = {
    val scoreMerge = compressorSolutions
      .flatMap(_.compressorScores.toMap)
      .groupBy(_._1)
      .map { case (k, v) =>
        k match {
          case "rr" => k -> v.map(_._2).sum / v.length
          case "hr" => k -> (stageInHeight - stageOutHeight).toDouble
          case _    => k -> v.map(_._2).sum
        }
      }
    scoreMerge.map { case (k, v) =>
      k match {
        case "re" =>
          k -> scoreMerge.getOrElse("br", 0.0) / scoreMerge.getOrElse(
            "cost",
            0.0
          )
        case _ => k -> v
      }
    }
  }

}

case class CompressorStepSolution(
    compressorName: String,
    width: Int,
    columnIndex: Int,
    compressorScores: ScoreIndicator = ScoreIndicator(0, 0, 0, 0, 0)
) {
  def getCompressor(
      complementHeap: Seq[Seq[Boolean]] = null
  ): CompressorGenerator =
    bitheap.getCompressor(
      compressorName,
      width,
      complementHeap
    )

  def vivadoUtilEstimation = getCompressor().vivadoUtilEstimation
}
