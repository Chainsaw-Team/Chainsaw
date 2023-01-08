package Chainsaw.examples

import Chainsaw.xilinx.VivadoUtil
import ilog.concert._
import ilog.cplex._

import scala.util.Random._

object CplexExample extends App {

  val cplex = new IloCplex()

  val varCount                    = 100000
  val variables: Array[IloIntVar] = cplex.intVarArray(varCount, 0, 1000)

  cplex.addLe(
    cplex.scalProd(variables, Array.tabulate(varCount)(_ => nextInt(1500))),
    15000
  )
  cplex.addLe(
    cplex.scalProd(variables, Array.tabulate(varCount)(_ => nextInt(1500))),
    15000
  )

  cplex.addMaximize(cplex.scalProd(variables, Array.fill(varCount)(1)))

  cplex.solve()
  //  variables.map(cplex.getValue).foreach(println)
  val budget = VivadoUtil(1000, 2000, 500, 800)
  val schemes = Seq(
    VivadoUtil(15, 50, 40, 30),
    VivadoUtil(10, 40, 50, 55),
    VivadoUtil(70, 90, 15, 35)
  )
  budget.solveBestScheme(schemes, Seq.tabulate(3)(i => i)).foreach(println)
}
