package Chainsaw.xilinx


case class VivadoUtil(
                       lut: Int,
                       ff: Int,
                       dsp: Int,
                       bram36: Int,
                       uram288: Int,
                       carry8: Int
                     ) {

  def getValues = Seq(lut, ff, dsp, bram36, uram288, carry8)

  def +(that: VivadoUtil) = VivadoUtil(
    this.getValues.zip(that.getValues).map { case (a, b) => a + b }
  )

  def *(k: Int) = VivadoUtil(this.getValues.map(_ * k))

  // to get percentage
  def /(that: VivadoUtil): Seq[Double] =
    this.getValues.zip(that.getValues).map { case (a, b) => a.toDouble / b }

  def <=(that: VivadoUtil): Boolean =
    this.getValues.zip(that.getValues).forall { case (a, b) => a.toDouble <= b }

  def >=(that: VivadoUtil): Boolean = that <= this

  def normalizedCost(clbPerDsp: Double): Double = dsp.toDouble max (lut / clbPerDsp)

  def isBetterThan(that: VivadoUtil, strategy: UtilStrategy, clbPerDsp: Double) = {

    def getDet(value: Double, thatValue: Double): Int = if (value < thatValue) 1 else if (value == thatValue) 0 else -1

    val dspBetter = getDet(this.dsp, that.dsp)
    val clbBetter = getDet(this.lut, that.lut)

    // TODO: generalize clbPerDsp
    val ratioBetter = getDet(this.normalizedCost(clbPerDsp), that.normalizedCost(clbPerDsp))

    /** judge whether a solution is better than another by multiple determinants with decresing priorities
     *
     * @param betters 1 for better, 0 for equally good, -1 for worst
     */
    def betterWithPriority(betters: Int*): Boolean = betters.reverse.zipWithIndex.map { case (better, i) => better << i }.sum > 0

    strategy match {
      case DspFirst => betterWithPriority(dspBetter, clbBetter, ratioBetter)
      case ClbFirst => betterWithPriority(clbBetter, dspBetter, ratioBetter)
      case RatioFirst => betterWithPriority(ratioBetter, dspBetter, clbBetter)
    }
  }

  def showInt(value: Int) =
    if (value == Int.MaxValue) "unlimited" else value.toString

  override def toString = {
    Seq("lut", "ff", "dsp", "bram36", "uram288", "carry8")
      .map(_.toUpperCase)
      .zip(getValues.map(showInt))
      .map { case (name, value) => s"$name = $value" }
      .mkString(" ")
  }

  object VivadoUtil {
    def apply(values: Seq[Int]): VivadoUtil =
      new VivadoUtil(values(0), values(1), values(2), values(3), values(4), values(5))
  }
}

object VivadoUtilRequirement {
  val limit = Int.MaxValue

  def apply(
             lut: Int = limit,
             ff: Int = limit,
             dsp: Int = limit,
             bram36: Int = limit,
             uram288: Int = limit,
             carry8: Int = limit
           ) =
    VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
}

sealed trait UtilStrategy

object DspFirst extends UtilStrategy

object ClbFirst extends UtilStrategy

object RatioFirst extends UtilStrategy