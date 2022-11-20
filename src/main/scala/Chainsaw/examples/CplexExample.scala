package Chainsaw.examples

import Chainsaw.xilinx.{VivadoUtil, VivadoUtilRequirement}
import ilog.cplex._
import ilog.concert._
import scala.util.Random._

object CplexExample extends App {

  val cplex = new IloCplex()

  val varCount                    = 100000
  val variables: Array[IloIntVar] = cplex.intVarArray(varCount, 0, 1000)

  cplex.addLe(cplex.scalProd(variables, Array.tabulate(varCount)(_ => nextInt(1500))), 15000)
  cplex.addLe(cplex.scalProd(variables, Array.tabulate(varCount)(_ => nextInt(1500))), 15000)

  cplex.addMaximize(cplex.scalProd(variables, Array.fill(varCount)(1)))

  cplex.solve()
//  variables.map(cplex.getValue).foreach(println)
  val budget  = VivadoUtilRequirement(1000, 2000, 500, 800)
  val schemes = Seq(VivadoUtilRequirement(15, 50, 40, 30), VivadoUtilRequirement(10, 40, 50, 55), VivadoUtilRequirement(70, 90, 15, 35))
  budget.solveBestScheme(schemes, Seq.tabulate(3)(i => i)).foreach(println)
}
