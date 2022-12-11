package Chainsaw.deprecated

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import sourcecode.Text.generate

import collection.JavaConverters._
import Chainsaw._

object AutoPipeline {
  /** in-place rewrite method that allocate latencies on edges which: 1. guarantee enough latency spaces for vertices 2. has minimum overall latency for graph, solved by linear programming
   *
   * @return dag with a valid solution and weights on edges
   * @see ''Parhi, Keshab K.. “Vlsi Digital Signal Processing Systems: Design And Implementation.” (2007).'' Chapter 4
   */
  def apply(dag: Dag, sources: Option[Seq[DagVertex]] = None,
            targets: Option[Seq[DagVertex]] = None,
            maximumLatency: Int = 500): Dag = {

    implicit val refDag: Dag = dag

    // prepare
    //    dag.makeComb()

    // declare model
    implicit val model: MPModel = MPModel(SolverLib.oJSolver)

    // declare vertices as variables
    val vertices = dag.vertexSet().asScala.toSeq
    val variableMap: Map[DagVertex, MPFloatVar] = vertices.zipWithIndex.map { case (vertex, i) =>
      vertex -> MPFloatVar(s"r$i", 0, maximumLatency)
    }.toMap

    // retiming region is between ins and outs
    val ins = sources.getOrElse(dag.inputs)
    val outs = targets.getOrElse(dag.outputs)

    // the all-in-one target function
    add(variableMap(dag.inputs.head) := 0) // set value 0 for the first input port

    val coeffs = outs.map(_ => 1.0) ++ ins.map(_ => -1.0)
    val expr = (outs ++ ins).zip(coeffs)
      .map { case (v, coeff) => variableMap(v) * coeff }
      .reduce(_ + _) // \Sigma outs - \Sigma ins

    minimize(expr)

    // setting constraints for inputs/outputs
    val inTimes = dag.inputTimes.getOrElse(dag.inputs.map(_ => 0))
    val outTimes = dag.outputTimes.getOrElse(dag.outputs.map(_ => 0))

    dag.inputs.zip(inTimes).prevAndNext { case ((vPre, tPre), (vNext, tNext)) =>
      add(variableMap(vPre) - variableMap(vNext) := tPre - tNext)
    }

    dag.outputs.zip(outTimes).prevAndNext { case ((vPre, tPre), (vNext, tNext)) =>
      add(variableMap(vPre) - variableMap(vNext) := tPre - tNext)
    }

    // add feasibility constraints between vertices(modules)
    dag.edgeSet().asScala.toSeq
      .filter(e => !outs.contains(e.source) && !ins.contains(e.target)) // outs won't be constrained by following vertices
      .foreach { e =>
        val sourceVar = variableMap(e.source)
        val targetVar = variableMap(e.target)
        add(variableMap(e.source) - variableMap(e.target) <:= e.targetPort.relativeTime - e.sourcePort.relativeTime)
        if (verbose >= 1) logger.info(s"${e.source} - ${e.target} <= ${e.targetPort.relativeTime} - ${e.sourcePort.relativeTime}")
      }

    // solve the LP problem
    start()

    // rounding, toInt is equivalent to ceil, which is not appropriate
    import scala.math.round
    val minValue = variableMap.values.map(_.value.get).min
    val solution = vertices.map { v =>
      val variable = variableMap(v)
      v -> round(variable.value.getOrElse(-1.0) - minValue).toInt
    }.toMap
    dag.retimingInfo = solution
    logger.info(
      s"\n----retiming report of ${refDag.name}----" +
        s"\n\tsolution latency = ${solution(outs.head) - solution(ins.head)}"
    )

    release()

    dag.retiming(solution)
    dag
  }
}

