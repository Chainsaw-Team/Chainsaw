package Chainsaw

import java.io.File

package object dag {

  sealed trait Direction

  object In extends Direction

  object Out extends Direction

  implicit def gen2vertex(gen: ChainsawGenerator)(implicit dag: Dag): DagVertex = DagVertex(gen)

  implicit class ChainsawGeneratorUtil(gen: ChainsawGenerator) {

    // by this, we don't need to implement asVertex inside ChainsawGenerator, avoiding two-way dependence
    def asVertex(implicit dag: Dag) = DagVertex(gen)

  }

  val dagFigDir = {
    val dir = new File("src/main/resources/dfgGenerated")
    dir.mkdirs()
    dir
  }
}
