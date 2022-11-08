package Chainsaw.xilinx

case class VivadoUtil(
                       lut: Int,
                       ff: Int,
                       dsp: Int,
                       bram36: Int,
                       carry8: Int = 0
                     ) {

  def getValues = Seq(lut, ff, dsp, bram36, carry8)

  def +(that: VivadoUtil) = VivadoUtil(
    this.getValues.zip(that.getValues).map { case (a, b) => a + b }
  )

  def *(k: Int) = VivadoUtil(this.getValues.map(_ * k))

  // to get percentage
  def /(that: VivadoUtil) =
    this.getValues.zip(that.getValues).map { case (a, b) => a.toDouble / b }

  def <=(that: VivadoUtil) =
    this.getValues.zip(that.getValues).forall { case (a, b) => a.toDouble <= b }

  def >=(that: VivadoUtil) = that <= this

  def showInt(value: Int) =
    if (value == Int.MaxValue) "unlimited" else value.toString

  override def toString = {
    Seq("lut", "ff", "dsp", "bram36", "carry8")
      .map(_.toUpperCase)
      .zip(getValues.map(showInt))
      .map { case (name, value) => s"$name = $value" }
      .mkString(" ")
  }

  object VivadoUtil {
    def apply(values: Seq[Int]): VivadoUtil =
      new VivadoUtil(values(0), values(1), values(2), values(3), values(4))
  }
}

object VivadoUtilRequirement {
  val limit = Int.MaxValue

  def apply(
             lut: Int = limit,
             ff: Int = limit,
             dsp: Int = limit,
             bram36: Int = limit,
             carry8: Int = limit
           ) =
    VivadoUtil(lut, ff, dsp, bram36, carry8)
}
