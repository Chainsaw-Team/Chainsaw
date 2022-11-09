package Chainsaw.dag

import Chainsaw._

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ArrayBuffer

object ToPng {

  def apply(dag: Dag, pngName: String = null): Unit = {

    implicit val refDag: Dag = dag

    type V = DagVertex
    type E = DagEdge
    type Port = DagPort

    import _root_.com.mxgraph.layout._
    import _root_.com.mxgraph.util.mxCellRenderer
    import org.jgrapht.ext.JGraphXAdapter

    val graphAdapter = new JGraphXAdapter[V, E](dag) // manager which store the information for rendering

    val vertexMap = graphAdapter.getVertexToCellMap
    val edgeMap = graphAdapter.getEdgeToCellMap

    // set styles of vertices/edges
    def colorVertices(vertices: Seq[V], color: String): Array[AnyRef] = {
      val targets = vertices.map(vertexMap.get(_)).toArray
      graphAdapter.setCellStyle(s"fillColor=#$color", targets.asInstanceOf[Array[Object]])
    }

    def colorEdges(edges: Seq[E], color: String): Array[AnyRef] = {
      val targets = edges.map(edgeMap.get(_)).toArray
      graphAdapter.setCellStyle(s"strokeColor=#$color", targets.asInstanceOf[Array[Object]])
    }

    /** --------
     * coloring
     * -------- */
    val postVertices = ArrayBuffer[V]()
    val postEdges = ArrayBuffer[E]()

    var currents: Seq[E] = dag.outputs.flatMap(_.incomingEdges)
    while (currents.nonEmpty) {
      val drivingVertices = currents.map(_.source).filter(v => v.isIo)
      val drivingEdges = drivingVertices.flatMap(_.incomingEdges)
      postEdges ++= drivingEdges
      postVertices ++= drivingVertices
      currents = drivingEdges
    }

    colorVertices(postVertices, "CCCC00")
    colorEdges(postEdges, "CCCC00")

    /** --------
     * constructing layout
     * -------- */

    // using the mxGraph built-in CompactTreeLayout as our beginning
    val layout = new mxCompactTreeLayout(graphAdapter, false, true)
    // settings
    layout.setMoveTree(true)
    layout.setEdgeRouting(false)
    layout.setResizeParent(false)
    layout.setLevelDistance(5)

    layout.execute(graphAdapter.getDefaultParent) // starts from a tree layout

    // customization, according to the pipeline information
    // TODO: better layout algo

    def doLayoutByRetimingInfo(): Unit = {
      val timeMax = dag.retimingInfo.values.max
      val initialView = layout.getGraph.getView

      def adjustY(v: V, y: Double) = {
        val cell = vertexMap.get(v)
        layout.setVertexLocation(cell, initialView.getState(cell).getX, y)
      }

      val pipelineGap = 50
      dag.inputs.zipWithIndex.foreach { case (v, _) => adjustY(v, 0) }
      dag.outputs.zipWithIndex.foreach { case (v, _) => adjustY(v, (timeMax + 2) * pipelineGap) }

      dag.retimingInfo
        .filterNot(_._1.isIo)
        .groupBy(_._2).foreach { case (_, vToInt) => vToInt.foreach { case (v, i) => adjustY(v, (i + 1) * pipelineGap) }
      }
    }

    //    doLayoutByRetimingInfo

    /** --------
     * png generation
     * -------- */
    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val fileName = if (pngName == null) dag.name else pngName
    val imgFile = new File(dagFigDir, s"$fileName.png")
    logger.info(s"view png generated for ${dag.name} in \n${imgFile.getAbsoluteFile}")
    ImageIO.write(image, "PNG", imgFile)
  }
}
