package Chainsaw.examples

import ilog.cplex._
import ilog.concert._

object CplexExample extends App {

  val cplex = new IloCplex()

  val varCount = 3
  val variables: Array[IloIntVar] = cplex.intVarArray(varCount, 0, 1000)

  cplex.addLe(cplex.scalProd(variables, Array(15, 11, 0)), 100)
  cplex.addLe(cplex.scalProd(variables, Array(0, 11, 15)), 100)

  cplex.addMaximize(cplex.scalProd(variables, Array(1, 1, 1)))

  cplex.solve()
  variables.map(cplex.getValue).foreach(println)
}
