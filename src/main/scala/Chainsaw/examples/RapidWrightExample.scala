package Chainsaw.examples
import com.xilinx.rapidwright.design.tools.RelocationTools
import com.xilinx.rapidwright.design.{Design, DesignTools, Net, SiteInst}
import com.xilinx.rapidwright.device.SiteTypeEnum
import com.xilinx.rapidwright.tests.CodePerfTracker
import org.jgrapht.graph._

import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

sealed trait RelocateStrategy
object MINAREA extends RelocateStrategy

object RelocateHierarchy {
  def main(args: Array[String]): Unit = {
    if (args.length != 5 && args.length != 6) {
      System.out.println(
        "USAGE: <input_dcp> <hierarchical_path> <tile_col_offset> <tile_row_offset> <output_dcp> [comma separated list of additional SiteTypeEnums to relocate]"
      )
      return
    }
    val t: CodePerfTracker      = new CodePerfTracker("Relocate Design", true).start("Loading design")
    val dcpName: String         = args(0)
    val hierarchyPrefix: String = args(1)
    val colOffset: Int          = args(2).toInt
    val rowOffset: Int          = args(3).toInt

    val design: Design = Design.readCheckpoint(dcpName, CodePerfTracker.SILENT)
    t.stop.start("Relocation")
    val customSet: util.Set[SiteTypeEnum] = RelocationTools.defaultSiteTypes
    if (args.length == 6) for (siteTypeEnum <- args(5).split(",")) {
      customSet.add(SiteTypeEnum.valueOf(siteTypeEnum))
    }
    if (!RelocationTools.relocate(design, hierarchyPrefix, colOffset, rowOffset, customSet))
      throw new RuntimeException("ERROR: Relocation failed")
    t.stop.start("Write DCP")
    design.setAutoIOBuffers(false)
    design.writeCheckpoint(args(4), CodePerfTracker.SILENT)
    t.stop.printSummary()
  }
}

object RelocatePartial {
  def designToGraph(design: Design): DirectedMultigraph[SiteInst, Net] = {
    val graph = new DirectedMultigraph[SiteInst, Net](classOf[Net])
    design.getNets.asScala.foreach { net =>
      val src = net.getSource
      if (src != null) graph.addVertex(src.getSiteInst)
      net.getSinkPins.asScala.foreach { sinkPin =>
        if (sinkPin != null) graph.addVertex(sinkPin.getSiteInst)
        if (src != null && sinkPin != null && !graph.containsEdge(src.getSiteInst, sinkPin.getSiteInst)) {
          if (src.getSiteInst != sinkPin.getSiteInst) {
            graph.addEdge(src.getSiteInst, sinkPin.getSiteInst, net)
//            println(s"${src.getSiteInst} -> ${sinkPin.getSiteInst}")
          }
        }
      }
    }
    var tempSiteInst  = graph.vertexSet().asScala.toSeq.filter(inst => graph.inDegreeOf(inst) == 0)
    val siteInstances = ArrayBuffer[Seq[SiteInst]]()
    siteInstances += tempSiteInst
//    while (!tempSiteInst.forall(graph.outgoingEdgesOf(_).asScala.isEmpty)) {
//      tempSiteInst = tempSiteInst.flatMap(inst => graph.outgoingEdgesOf(inst).asScala.map(graph.getEdgeTarget))
//      siteInstances += tempSiteInst
//      println(tempSiteInst.map(_.getName))
//    }

    // TODO: Perhaps restricting siteInst in the next stage do not include siteInst in the previous stage can avoid this problem
    val firstStageSiteInst = graph.vertexSet().asScala.toSeq.filter(inst => graph.inDegreeOf(inst) == 0)
    val secondStageSiteInst =
      firstStageSiteInst.flatMap(inst => graph.outgoingEdgesOf(inst).asScala.map(graph.getEdgeTarget))
    val thirdStageSiteInst =
      secondStageSiteInst.flatMap(inst => graph.outgoingEdgesOf(inst).asScala.map(graph.getEdgeTarget))
    val fourthStageSiteInst =
      thirdStageSiteInst.flatMap(inst => graph.outgoingEdgesOf(inst).asScala.map(graph.getEdgeTarget))
    val fifthStageSiteInst =
      fourthStageSiteInst.flatMap(inst => graph.outgoingEdgesOf(inst).asScala.map(graph.getEdgeTarget))
    val sixthStageSiteInst =
      fifthStageSiteInst.flatMap(inst => graph.outgoingEdgesOf(inst).asScala.map(graph.getEdgeTarget))
    val seventhStageSiteInst =
      sixthStageSiteInst.flatMap(inst => graph.outgoingEdgesOf(inst).asScala.map(graph.getEdgeTarget))
    val eighthStageSiteInst =
      seventhStageSiteInst.flatMap(inst => graph.outgoingEdgesOf(inst).asScala.map(graph.getEdgeTarget))
    println(firstStageSiteInst.map(_.getSite))
    println(secondStageSiteInst.map(_.getSite))
    println(thirdStageSiteInst.map(_.getSite))
    println(fourthStageSiteInst.map(_.getSite))
    println(fifthStageSiteInst.map(_.getSite))
    println(sixthStageSiteInst.map(_.getSite))
    println(seventhStageSiteInst.map(_.getSite))
    println(eighthStageSiteInst.map(_.getSite))
//    siteInstances.foreach(println)
    graph
  }

  def apply(design: Design, siteInsts: Seq[SiteInst], tileColOffset: Int, tileRowOffset: Int) = {
    val successfulRelocate = RelocationTools.relocate(design, siteInsts.asJava, tileColOffset, tileRowOffset)
    design.getNets.asScala.foreach { net =>
      net.getPins.asScala.foreach { pin =>
        if (!siteInsts.contains(pin.getSiteInst)) {
          net.unroute();
          val srcNode = net.getSource.getRouteNode
          net.getSinkPins.asScala.foreach { sink =>
            val snkNode = sink.getRouteNode
            val path    = DesignTools.findRoutingPath(srcNode, snkNode)
            if (path == null) {
              System.out.println(
                "INFO: Unrouting Net '" + net.getName + "' since src SitePinInst '" +
                  net.getSource + "' don't have route path to '" + sink
              );
            } else {
              net.getPIPs.addAll(path);
            }
          }
          net.unlockRouting();
        }
      }
    }
    design.getSiteInsts.asScala.foreach(inst => inst.setName(inst.getSite.getName))
    successfulRelocate
  }

  def apply(
      design: Design,
      siteInsts: Seq[SiteInst] = null,
      strategy: RelocateStrategy
  ): DirectedMultigraph[SiteInst, Net] = {
    designToGraph(design)
  }

  def main(args: Array[String]): Unit = {
    val design    = Design.readCheckpoint("BitHeapCompressor_N2001106564_inferred_after_route_phys_opt.dcp")
    val netList   = design.getNetlist
    val top       = netList.getTopCell
    val cells     = design.getCells.asScala
    val nets      = design.getNets.asScala
    val siteInsts = design.getSiteInsts.asScala.toSeq

    val toRelocate = new ArrayBuffer[SiteInst]()
    toRelocate += siteInsts.head
//    println(
//      cells.zip(cells.map(_.getSite)).map { case (cell, site) => cell.getName + "<-in->" + site.getName }.mkString("\n")
//    )
//    println(
//      nets
//        .map(net => net.getName)
//        .mkString("\n") + s"len=${nets.size}"
//    )
//    println(cells.mkString("\n"))
//    println(nets.map(net => s"${net.getName} -> ${net.getSource}").mkString("\n"))
    //        System.out.println(siteInsts.get(0).getTile().getTileXCoordinate());
    RelocatePartial(design, siteInsts = null, strategy = MINAREA)
//    RelocatePartial(design, toRelocate, -5, -10)
    //        design.routeSites();
//    design.writeCheckpoint("BitHeapCompressor_N2001106564_inferred_after_synth2.dcp")
    System.out.println();
  }
}
