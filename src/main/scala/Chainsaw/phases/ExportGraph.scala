package Chainsaw.phases

// utils for Customized Spinal Elaboration

import Chainsaw._
import com.mxgraph.layout._
import com.mxgraph.util.mxCellRenderer
import org.jgrapht._
import org.jgrapht.ext.JGraphXAdapter
import spinal.core._
import spinal.core.internals.Expression

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.JavaConverters._
import scala.collection.mutable

/** unified graph visualization interface
 *
 */
object ExportGraph {
  def apply[V, E](graph: Graph[V, E], name: String) = {
    val graphAdapter = new JGraphXAdapter[V, E](graph) // manager which store the information for rendering
    val layout = new mxCompactTreeLayout(graphAdapter, false, true)
    layout.setMoveTree(true)
    layout.setEdgeRouting(false)
    layout.setResizeParent(false)
    layout.setLevelDistance(5)

    layout.execute(graphAdapter.getDefaultParent) // starts from a tree layout
    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val imgFile = new File(dagOutputDir, s"$name.png")
    logger.info(s"view the DFG of $name at ${imgFile.getAbsolutePath}")
    ImageIO.write(image, "PNG", imgFile)
  }
}