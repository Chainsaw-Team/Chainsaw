package Chainsaw.xilinx

import Chainsaw._
import Chainsaw.xilinx.VivadoUtil.UNKNOWN
import ilog.concert._
import ilog.cplex._

// TODO: resources should be double, rather than int
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
    this.getValues.zip(that.getValues).map { case (a, b) => if (a == -1 || b == -1) 0 else a + b }
  )

  def -(that: VivadoUtil) = VivadoUtil(
    this.getValues.zip(that.getValues).map { case (a, b) => if (a == -1 || b == -1) 0 else a - b }
  )

  def *(k: Int) = VivadoUtil(this.getValues.map(_ * k))

  // to get percentage
  def /(that: VivadoUtil): Seq[Double] =
    this.getValues.zip(that.getValues).map { case (a, b) => a.toDouble / b }

  def <(that: VivadoUtil): Boolean =
    this.getValues.zip(that.getValues).forall { case (a, b) => a.toDouble < b }

  def >(that: VivadoUtil): Boolean = that < this

  def <=(that: VivadoUtil): Boolean =
    this.getValues.zip(that.getValues).forall { case (a, b) => a.toDouble <= b }

  def >=(that: VivadoUtil): Boolean = that <= this

  def normalizedCost(clbPerDsp: Double): Double = dsp.toDouble max (lut / clbPerDsp)

  def solveBestScheme(schemes: Seq[VivadoUtil], solveVars: Seq[Int]): Array[Int] = {
    val cplex = new IloCplex()

    val upBounds = schemes
      .map(scheme => solveVars.map(v => scheme.getValues(v)))
      .map { consumes =>
        consumes.zip(solveVars.map(v => this.getValues(v))).map { case (consume, budget) => budget / consume }.min
      }

    val weights = solveVars.map(i => schemes.map(scheme => scheme.getValues(i)).toArray)
    val budgets = solveVars.map(i => this.getValues(i))

    val variables: Array[IloIntVar] = upBounds.flatMap(upBound => cplex.intVarArray(1, 0, upBound)).toArray

    var equation = ""
    weights.zip(budgets).foreach { case (weight, budget) =>
      equation += weight.zip(Seq.tabulate(weights.length)(i => s"x$i")).map { case (i, str) => s"$i * $str" }.mkString(" + ") + s" <= $budget\n"
      cplex.addLe(cplex.scalProd(variables, weight), budget)
    }

    cplex.addMaximize(cplex.scalProd(variables, Array.fill(variables.length)(1)))
    cplex.solve()

    val ret       = variables.map(cplex.getValue).map(_.toInt)
    val takes     = ret.zip(schemes).map { case (i, solution) => s"take $i X $solution" }.mkString("\n")
    val util      = ret.zip(schemes).map { case (i, solution) => solution * i }.reduce(_ + _)
    val utilInAll = s"LUT: ${util.lut} / ${this.lut}, DSP: ${util.dsp} / ${this.dsp}"

    logger.info(
      s"\n----schemes search report----" +
        s"\nconstraints:\n$equation" +
        s"maximize: \n${variables.indices.map(i => s"x$i").mkString(" + ")}" +
        s"\nresults: \n$takes" +
        s"\nutils: \n$utilInAll"
    )
    ret
  }

  def cost(considerFF: Boolean): Double = {
    val lutCost       = if (lut != UNKNOWN) lut.toDouble else 0.0
    val carry8Cost    = if (carry8 != UNKNOWN) carry8 * 8.0 else 0.0
    val ffCost        = if (ff != UNKNOWN) ff.toDouble / 2 else 0.0
    val considerTable = Seq(lutCost, carry8Cost, ffCost)
    if (considerTable.forall(_ == 0.0)) throw new IllegalArgumentException("no estimation exist!")
    if (considerFF) considerTable.max else considerTable.init.max
  }

  def toRequirement = VivadoUtil(getValues.map(value => if (value == -1) UNKNOWN else value))

  private def toIntString(value: Int) =
    if (value == UNKNOWN || value == -1) "???" else value.toString

  override def toString = {
    Seq("lut", "ff", "dsp", "bram36", "uram288", "carry8")
      .map(_.toUpperCase)
      .zip(getValues.map(toIntString))
      .map { case (name, value) => s"$name = $value" }
      .mkString(" ")
  }

  object VivadoUtil {
    def apply(values: Seq[Int]): VivadoUtil =
      new VivadoUtil(values(0), values(1), values(2), values(3), values(4), values(5))
  }
}

object VivadoUtil {
  val UNKNOWN = Int.MaxValue
}

object VivadoUtilRequirement {
  val limit = Int.MaxValue

  def apply(
      lut: Int     = VivadoUtil.UNKNOWN,
      ff: Int      = VivadoUtil.UNKNOWN,
      dsp: Int     = VivadoUtil.UNKNOWN,
      bram36: Int  = VivadoUtil.UNKNOWN,
      uram288: Int = VivadoUtil.UNKNOWN,
      carry8: Int  = VivadoUtil.UNKNOWN
  ) =
    VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
}

object VivadoUtilEstimation {
  def apply(
      lut: Int     = -1,
      ff: Int      = -1,
      dsp: Int     = -1,
      bram36: Int  = -1,
      uram288: Int = -1,
      carry8: Int  = -1
  ) =
    VivadoUtil(lut, ff, dsp, bram36, uram288, carry8)
}
