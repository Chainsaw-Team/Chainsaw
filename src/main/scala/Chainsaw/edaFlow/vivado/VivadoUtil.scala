package Chainsaw.edaFlow.vivado

import Chainsaw._

/** a class to record the utilization of a module, for all value
  */
case class VivadoUtil(
    lut: Double,
    ff: Double,
    dsp: Double,
    bram36: Double,
    uram288: Double,
    carry8: Double
) {

  def withLut(lut: Double) = VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
  def withFf(ff: Double)   = VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
  def withDsp(dsp: Double) = VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
  def withBram(bram36: Double) =
    VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
  def withUram(uram288: Double) =
    VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
  def withCarry(carry8: Double) =
    VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)

  /** -------- util arithmetic
    * --------
    */
  def getValues = Seq(lut, ff, dsp, bram36, uram288, carry8)

  // +,-,*,/
  def +(that: VivadoUtil) = VivadoUtil(
    this.getValues.zip(that.getValues).map { case (a, b) => a + b }
  )

  def -(that: VivadoUtil) = VivadoUtil(
    this.getValues.zip(that.getValues).map { case (a, b) => a - b }
  )

  def *(k: Int)    = VivadoUtil(this.getValues.map(_ * k))
  def *(k: Double) = VivadoUtil(this.getValues.map(_ * k))

  // to get percentage
  def /(that: VivadoUtil): Seq[Double] =
    this.getValues.zip(that.getValues).map { case (a, b) => a / b }

  // comparison
  // carry8 is not considered as it won't be specified in many cases
  def <(that: VivadoUtil): Boolean =
    this.getValues.zip(that.getValues).forall { case (a, b) => a < b }

  def >(that: VivadoUtil): Boolean = that < this

  def <=(that: VivadoUtil): Boolean =
    this.getValues.zip(that.getValues).forall { case (a, b) => a <= b }

  def >=(that: VivadoUtil): Boolean = that <= this

  // regard resources in CLB as one
  def clbCost(isPipeline: Boolean): Double = {
    val considerTable = Seq(lut, carry8 * 8, ff / 2)
    if (considerTable.forall(_ == 0.0))
      throw new IllegalArgumentException("no estimation exist!")
    // TODO: /8 for clb?
    if (isPipeline) considerTable.max else considerTable.init.max
  }

  override def toString = {
    Seq("lut", "ff", "dsp", "bram36", "uram288", "carry8")
      .map(_.toUpperCase)
      .zip(getValues.map(_.toString))
      .map { case (name, value) => s"$name = $value" }
      .mkString(" ")
  }
}

object VivadoUtil {
  def apply(values: Seq[Double]): VivadoUtil = {
    require(values.forall(_ >= 0))
    new VivadoUtil(
      values(0),
      values(1),
      values(2),
      values(3),
      values(4),
      values(5)
    )
  }
  def apply(
      lut: Double     = 0,
      ff: Double      = 0,
      dsp: Double     = 0,
      bram36: Double  = 0,
      uram288: Double = 0,
      carry8: Double  = 0
  ) = new VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
}
