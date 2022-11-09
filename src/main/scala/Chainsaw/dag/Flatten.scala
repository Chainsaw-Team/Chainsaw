package Chainsaw.dag

import scala.collection.JavaConverters._

object Flatten {
  def apply(dag: Dag): Dag = {
    implicit val refDag: Dag = dag

    if (dag.vertexSet().asScala.exists(_.gen.isInstanceOf[Dag])) {
      dag.vertexSet().asScala.filter(_.gen.isInstanceOf[Dag])
        .map(_.gen) // squeeze out generators appear repeatedly
        .foreach(subGraph => Flatten(subGraph.asInstanceOf[Dag]))
    }

    var step = 0

    dag.vertexSet().asScala.filter(_.gen.isInstanceOf[Dag])
      .foreach { v =>
        step += 1
        dag.addGraphBetween(v.gen.asInstanceOf[Dag], v.sourcePorts, v.targetPorts)
        dag.removeVertex(v)
      }

    dag
  }
}
