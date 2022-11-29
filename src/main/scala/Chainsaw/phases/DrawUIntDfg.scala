package Chainsaw.phases

import Chainsaw.dag.dagFigDir
import Chainsaw.logger
import com.mxgraph.layout.mxCompactTreeLayout
import com.mxgraph.util.mxCellRenderer
import org.jgrapht.ext.JGraphXAdapter
import org.jgrapht.graph.{DefaultEdge, DirectedWeightedMultigraph}
import spinal.core._
import spinal.core.internals._

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

class DrawUIntDfg() extends Phase{
  override def impl(pc: PhaseContext): Unit = {

    val graph = new DirectedWeightedMultigraph[Expression, DefaultEdge](classOf[DefaultEdge])

    pc.topLevel.dslBody.walkStatements {
      case as: AssignmentStatement =>
        graph.addVertex(as.source)
        graph.addVertex(as.target)
        graph.addEdge(as.source, as.target)
        as.source.walkDrivingExpressions { e =>
          graph.addVertex(e)
          graph.addEdge(e, as.source)
        }
      case _ => // ignore
    }

    val graphAdapter = new JGraphXAdapter[Expression, DefaultEdge](graph) // manager which store the information for rendering
    val layout = new mxCompactTreeLayout(graphAdapter, false, true)
    layout.execute(graphAdapter.getDefaultParent) // starts from a tree layout
    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val imgFile = new File(dagFigDir, s"${pc.topLevel.definitionName}.png")
    logger.info(s"view the DFG of ${pc.topLevel.definitionName} at ${imgFile.getAbsolutePath}")
    ImageIO.write(image, "PNG", imgFile)

  }
  override def hasNetlistImpact = false
}

object DrawUIntDfg {
  class SmallKara extends Component {
    val dataIn = in Vec(UInt(32 bits), 2)
    val dataOut = out UInt (64 bits)

    val (aH, aL) = dataIn(0).splitAt(16)
    val (bH, bL) = dataIn(1).splitAt(16)

    val high = aH.asUInt * bH.asUInt
    val low = aL.asUInt * bL.asUInt
    val mid = (aH.asUInt * bL.asUInt) +^ (aL.asUInt * bH.asUInt)

    val ret = (high << 32) + (mid << 16) + low
    dataOut := ret
  }

  def main(args: Array[String]): Unit = {
    val config = SpinalConfig()
    config.addTransformationPhase(new DrawUIntDfg())
    config.generateVerilog(new SmallKara)
  }
}
