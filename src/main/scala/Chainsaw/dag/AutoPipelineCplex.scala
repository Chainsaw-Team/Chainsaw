package Chainsaw.dag

import Chainsaw._
import ilog.concert.IloNumVar
import ilog.cplex.IloCplex

import scala.collection.JavaConverters._

object AutoPipelineCplex {
  /** in-place rewrite method that allocate latencies on edges which: 1. guarantee enough latency spaces for vertices 2. has minimum overall latency for graph, solved by linear programming
   *
   * @return dag with a valid solution and weights on edges
   * @see ''Parhi, Keshab K.. “Vlsi Digital Signal Processing Systems: Design And Implementation.” (2007).'' Chapter 4
   */
  def apply(dag: Dag, sources: Option[Seq[DagVertex]] = None, targets: Option[Seq[DagVertex]] = None, maximumLatency: Int = 500): Dag = {

    implicit val refDag: Dag = dag

    // prepare
    dag.makeComb()

    // declare cplex model
    val cplex = new IloCplex()

    // declare vertices as variables
    val vertices = dag.vertexSet().asScala.toArray
    val lowerBounds = vertices.map(_ => 0.0)
    val upperBounds = vertices.map(_ => maximumLatency.toDouble)
    val variables: Array[IloNumVar] = cplex.numVarArray(vertices.length, lowerBounds, upperBounds)
    val variableMap = vertices.zip(variables).toMap // vertex -> variable
    val coeffForSub = Array(1.0, -1.0) // coeff for expression a - b

    def setDiffLe(a: IloNumVar, b: IloNumVar, value: Double) = { // set a - b <= value
      val expr = cplex.scalProd(Array(a, b), coeffForSub)
      cplex.addLe(expr, value)
    }

    def setDiffEq(a: IloNumVar, b: IloNumVar, value: Double) = { // set a - b = value
      val expr = cplex.scalProd(Array(a, b), coeffForSub)
      cplex.addEq(expr, value)
    }

    def setVar(a: IloNumVar, value: Double) = { // set a = value
      val expr = cplex.scalProd(Array(a), Array(1.0))
      cplex.addEq(expr, value)
    }

    // retiming region is between ins and outs
    val ins = sources.getOrElse(dag.inputs)
    val outs = targets.getOrElse(dag.outputs)

    // the all-in-one target function
    setVar(variableMap(dag.inputs.head), 0.0)
    val coeffs = (outs.map(_ => 1.0) ++ ins.map(_ => -1.0)).toArray
    val ios = (outs ++ ins).map(variableMap).toArray
    val expr = cplex.scalProd(ios, coeffs) // \Sigma outs - \Sigma ins
    cplex.addMinimize(expr)

    // setting constraints for inputs/outputs
    val inTimes = dag.inputTimes.getOrElse(dag.inputs.map(_ => 0))
    val outTimes = dag.outputTimes.getOrElse(dag.outputs.map(_ => 0))

    dag.inputs.zip(inTimes).prevAndNext { case ((vPre, tPre), (vNext, tNext)) =>
      setDiffEq(variableMap(vPre), variableMap(vNext), tPre - tNext)
    }

    dag.outputs.zip(outTimes).prevAndNext { case ((vPre, tPre), (vNext, tNext)) =>
      setDiffEq(variableMap(vPre), variableMap(vNext), tPre - tNext)
    }

    // add feasibility constraints between vertices(modules)
    dag.edgeSet().asScala.toSeq
      .filter(e => !outs.contains(e.source) && !ins.contains(e.target)) // outs won't be constrained by following vertices
      .foreach { e =>
        val sourceVar = variableMap(e.source)
        val targetVar = variableMap(e.target)
        val expr = cplex.scalProd(Array(sourceVar, targetVar), coeffForSub) // targetVar + targetRelative - (sourceVar + sourceRelative) >= 0
        cplex.addLe(expr, e.targetPort.relativeTime - e.sourcePort.relativeTime)
        if (verbose >= 1) logger.info(s"${e.source} - ${e.target} <= ${e.targetPort.relativeTime} - ${e.sourcePort.relativeTime}")
      }

    // solve the LP problem
    cplex.solve()

    // rounding, toInt is equivalent to ceil, which is not appropriate
    import scala.math.round
    val values = cplex.getValues(variables).map(round).map(_.toInt)
    val minValue = values.min
    val solution = vertices.zip(values.map(_ - minValue)).toMap
    dag.retimingInfo = solution
    logger.info(
      s"\n----retiming report of ${refDag.name}----" +
        s"\n\tsolution status = ${cplex.getStatus}" +
        s"\n\tsolution latency = ${solution(outs.head) - solution(ins.head)}"
    )
    cplex.end()

    println(s"coeffs: ${coeffs.mkString(" ")}")
    println(s"solution: ${ins.map(dag.retimingInfo).mkString(" ")}")
    println(s"solution: ${outs.map(dag.retimingInfo).mkString(" ")}")
    dag.retiming(solution)
    dag
  }
}

