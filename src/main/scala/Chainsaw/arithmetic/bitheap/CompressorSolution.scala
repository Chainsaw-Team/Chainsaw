package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic.{CompressorGenerator, bitheap}

import java.io._

/** this class is used to store the scores(evaluation indicators) about compressor applying to [[BitHeap]] or
  * [[BitHeapGroup]]
  * @param bitReduction
  *   the total number of bit reduction
  * @param heightReduction
  *   the total number of height reduction
  * @param reductionEfficiency
  *   the total reduction efficiency
  * @param reductionRatio
  *   the total reduction ratio
  * @param cost
  *   the total CLB cost
  */
case class ScoreIndicator(
    bitReduction: Int,
    heightReduction: Int,
    reductionEfficiency: Double,
    reductionRatio: Double,
    cost: Double
) {

  /** compare two compressor according to current strategy, if this [[ScoreIndicator]] is greater than or equal to that
    * [[ScoreIndicator]], will return true
    * @param that
    *   the [[ScoreIndicator]] will be compared with this [[ScoreIndicator]]
    * @return
    *   the compare result, if this [[ScoreIndicator]] is greater than or equal to that [[ScoreIndicator]], it should
    *   return true
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

  /** the inverse operate of [[>]]
    */
  def <=(that: ScoreIndicator)(implicit strategy: CompressionStrategy) =
    !(this > that)

  /** this method is used to transform the scores to a Map for making table
    * @return
    *   a Map contain the scores and its name
    */
  def toMap: Map[String, Double] = Map(
    "br"   -> bitReduction.toDouble,
    "cost" -> cost,
    "re"   -> reductionEfficiency,
    "rr"   -> reductionRatio,
    "hr"   -> heightReduction.toDouble
  )

  /** this method is used to get the string for [[ScoreIndicator]]'s visualization
    * @return
    *   the string for [[ScoreIndicator]]'s visualization
    */
  override def toString =
    s"ScoresIndicator(br = $bitReduction, hr = $heightReduction, re = $reductionEfficiency, rr = $reductionRatio)"
}

/** this class is used to store the solution of the [[BitHeap]] compress tree solver
  * @param stageSolutions
  *   the solutions of all stage solve in a compress tree solver
  */
@SerialVersionUID(42L)
case class CompressorFullSolution(stageSolutions: Seq[CompressorStageSolution]) extends Serializable with HardAlgo {

  /** this method is used to get the latency of this solution
    * @return
    *   the latency of this solution
    */
  def latency = stageSolutions.count(_.pipelined)

  /** this method is used to get the output height of final stage
    * @return
    *   the output height of final stage
    */
  def outHeight = if (stageSolutions.nonEmpty) stageSolutions.last.stageOutHeight else -1

  /** this method is used to get the clb utilization of this solution
    * @return
    *   the clb utilization of this solution
    */
  override def vivadoUtilEstimation =
    stageSolutions.map(_.vivadoUtilEstimation).reduce(_ + _)

  /** this method is used to get the bit reduction of this solution
    * @return
    *   the bit reduction of this solution
    */
  def bitReduction = stageSolutions.map(_.bitReduction).sum

  /** this method is used to store this solution to the given file directory, it will overwrite old file
    * @param file
    *   file directory which this solution will be stored
    */
  def save(file: File): Unit = {
    file.getParentFile.mkdir()
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(this)
    oos.close()
  }

  /** this method is used to count the scores of this solution and return the result Map contain the name and scores
    * @return
    *   the Map contain statistical scores according to this solution
    */
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

  /** this method is used to transform this solution to the table string, it's the visualization method of solution
    * using table format
    * @param rows
    *   the input raw data will be regarded as rows in the table string
    * @param tail
    *   the input raw data will be regarded as final row in the table string, Usually used to summarize the previous
    *   rows, it can be missing
    * @return
    *   the visualized table string of this solution using table format
    */
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

  /** override the toString method, it is used to visualize this solution, will list all compressor solution information
    * @return
    *   the visualized String of this solution
    */
  override def toString = {
    s"CompressorFullSolution:\n${stageSolutions.map(_.toString).mkString("\n")}"
  }

  /** this method is used to get the table format visualization string of this solution
    * @return
    *   the table format visualization string of this solution
    */
  def tableReport = {
    val rows = stageSolutions.map(_.scores)
    val tail = scores
    toTable(rows, tail)
  }

  /** this method is used to get the report string of this solution
    * @return
    *   the report string of this solution
    */
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
      s"\n$classReport" +
      s"\n$subReport" +
      s"\n$vivadoUtilEstimation" +
      s"\n--------output status--------" +
      s"\nheight = $outHeight, width = ???"
  }
}

object CompressorFullSolution {

  /** this method is used to load a solution from the given file directory
    * @param file
    *   the given file directory which will load a solution from
    * @return
    *   a CompressorFullSolution from the given file directory
    */
  def load(file: java.io.File): CompressorFullSolution = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    val ret = ois.readObject().asInstanceOf[CompressorFullSolution]
    ois.close()
    ret
  }
}

/** this class is used to store the one stage solution of the [[BitHeap]] compress tree solver
  * @param compressorSolutions
  *   the step solutions of the stage solution in a compress tree solver
  * @param stageInHeight
  *   the input [[BitHeap]] height of this stage
  * @param stageOutHeight
  *   the output [[BitHeap]] height of this stage
  * @param pipelined
  *   this is used indicate whether this stage solve is pipelined
  */
case class CompressorStageSolution(
    compressorSolutions: Seq[CompressorStepSolution],
    stageInHeight: Int,
    stageOutHeight: Int,
    pipelined: Boolean
) {

  /** this method is used to get the stageIndex for generating solution log information
    * @return
    *   the stageIndex of this [[CompressorStageSolution]]
    */
  def stageIndex = {
    require(
      compressorSolutions.forall(_.stageIndex == compressorSolutions.head.stageIndex),
      s"all compressorSolutions in a compressorStageSolution should have same stageIndex."
    )
    compressorSolutions.head.stageIndex
  }

  /** this method is used to get the clb utilization of this stage solution
    * @return
    *   the clb utilization of this stage solution
    */
  def vivadoUtilEstimation =
    compressorSolutions.map(_.vivadoUtilEstimation).reduce(_ + _)

  /** this method is used to get the bit reduction of this stage solution
    * @return
    *   the bit reduction of this stage solution
    */
  def bitReduction =
    compressorSolutions.map(_.compressorScores.bitReduction).sum

  /** this method is used to count the scores of this stage solution and return the result Map contain the name and
    * scores
    * @return
    *   the Map contain statistical scores according to this stage solution
    */
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

  /** override the toString method, it is used to visualize this stage solution, will list all compressor solution
    * information
    * @return
    *   the visualized String of this stage solution
    */
  override def toString: String =
    s"CompressorStageSolution:\n${compressorSolutions.map(_.toString).mkString("\n")}\nstageInHeight: $stageInHeight, stageOutHeight: $stageInHeight, pipelined: $pipelined, stageIndex: $stageIndex"

}

/** this class is used to store the one step solution of the [[BitHeap]] compress tree solver
  * @param compressorName
  *   the name of compressor in this step solution
  * @param width
  *   the width of compressor solution
  * @param columnIndex
  *   the start index covered by the compressor in this step solution * @param stageIndex the stage index for generating
  *   solution log information and reducing fan-out
  * @param compressorScores
  *   the scores of a step solution
  */
case class CompressorStepSolution(
    compressorName: String,
    width: Int,
    columnIndex: Int,
    stageIndex: Int,
    compressorScores: ScoreIndicator = ScoreIndicator(0, 0, 0, 0, 0)
) {

  /** this method is used to get [[CompressorGenerator]] by the complement information
    * @param complementHeap
    *   the complement information for getting correct compressor
    * @return
    *   the correct compressor according to this step solution and complement information
    */
  def getCompressor(
      complementHeap: Seq[Seq[Boolean]] = null
  ): CompressorGenerator =
    bitheap.getCompressor(
      compressorName,
      width,
      complementHeap
    )

  /** this method is used to get the clb utilization of this step solution
    * @return
    *   the clb utilization of this step solution
    */
  def vivadoUtilEstimation = getCompressor().vivadoUtilEstimation

  /** override the toString method, it is used to visualize this step solution
    * @return
    *   the visualized String of this step solution
    */
  override def toString: String =
    s"CompressorStepSolution -> compressor: $compressorName, width: $width, columnIndex: $columnIndex, compressorScores: $compressorScores"
}
