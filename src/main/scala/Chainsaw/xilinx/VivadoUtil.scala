package Chainsaw.xilinx

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
  def clbCost(considerFF: Boolean): Double = {
    val considerTable = Seq(lut, carry8 * 8, ff / 2)
    if (considerTable.forall(_ == 0.0))
      throw new IllegalArgumentException("no estimation exist!")
    // TODO: /8 for clb?
    if (considerFF) considerTable.max else considerTable.init.max
  }

  override def toString = {
    Seq("lut", "ff", "dsp", "bram36", "uram288", "carry8")
      .map(_.toUpperCase)
      .zip(getValues.map(_.toString))
      .map { case (name, value) => s"$name = $value" }
      .mkString(" ")
  }

  /** -------- DSE methods
    * --------
    */
  // TODO: reimplement this by open-source libraries
  import ilog.concert._
  import ilog.cplex._
  def solveBestScheme(
      schemes: Seq[VivadoUtil],
      solveVars: Seq[Int]
  ): Array[Int] = {
    val cplex = new IloCplex()

    val upBounds = schemes
      .map(scheme => solveVars.map(v => scheme.getValues(v)))
      .map { consumes =>
        consumes
          .zip(solveVars.map(v => this.getValues(v)))
          .map { case (consume, budget) => budget / consume }
          .min
      }

    val weights =
      solveVars.map(i =>
        schemes.map(scheme => scheme.getValues(i).toInt).toArray
      )
    val budgets = solveVars.map(i => this.getValues(i))

    val variables: Array[IloIntVar] =
      upBounds
        .flatMap(upBound => cplex.intVarArray(1, 0, upBound.toInt))
        .toArray

    var equation = ""
    weights.zip(budgets).foreach { case (weight, budget) =>
      equation += weight
        .zip(Seq.tabulate(weights.length)(i => s"x$i"))
        .map { case (i, str) => s"$i * $str" }
        .mkString(" + ") + s" <= $budget\n"
      cplex.addLe(cplex.scalProd(variables, weight), budget)
    }

    cplex.addMaximize(
      cplex.scalProd(variables, Array.fill(variables.length)(1))
    )
    cplex.solve()

    val ret = variables.map(cplex.getValue).map(_.toInt)
    val takes = ret
      .zip(schemes)
      .map { case (i, solution) => s"take $i X $solution" }
      .mkString("\n")
    val util =
      ret.zip(schemes).map { case (i, solution) => solution * i }.reduce(_ + _)
    val utilInAll =
      s"LUT: ${util.lut} / ${this.lut}, DSP: ${util.dsp} / ${this.dsp}"

    logger.info(
      s"\n----schemes search report----" +
        s"\nconstraints:\n$equation" +
        s"maximize: \n${variables.indices.map(i => s"x$i").mkString(" + ")}" +
        s"\nresults: \n$takes" +
        s"\nutils: \n$utilInAll"
    )
    ret
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
