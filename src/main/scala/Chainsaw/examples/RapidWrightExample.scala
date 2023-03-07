import com.xilinx.rapidwright
import com.xilinx.rapidwright.design.Design
import com.xilinx.rapidwright.design.tools.RelocationTools
import com.xilinx.rapidwright.device.SiteTypeEnum
import com.xilinx.rapidwright.tests.CodePerfTracker

import java.util
import java.util.Set

object RelocateHierarchy {
  def main(args: Array[String]): Unit = {
    if (args.length != 5 && args.length != 6) {
      System.out.println("USAGE: <input_dcp> <hierarchical_path> <tile_col_offset> <tile_row_offset> <output_dcp> [comma separated list of additional SiteTypeEnums to relocate]")
      return
    }
    val t: CodePerfTracker = new CodePerfTracker("Relocate Design", true).start("Loading design")
    val dcpName: String = args(0)
    val hierarchyPrefix: String = args(1)
    val colOffset: Int = args(2).toInt
    val rowOffset: Int = args(3).toInt

    val design: Design = Design.readCheckpoint(dcpName, CodePerfTracker.SILENT)
    t.stop.start("Relocation")
    val customSet: util.Set[SiteTypeEnum] = RelocationTools.defaultSiteTypes
    if (args.length == 6) for (siteTypeEnum <- args(5).split(",")) {
      customSet.add(SiteTypeEnum.valueOf(siteTypeEnum))
    }
    if (!RelocationTools.relocate(design, hierarchyPrefix, colOffset, rowOffset, customSet)) throw new RuntimeException("ERROR: Relocation failed")
    t.stop.start("Write DCP")
    design.setAutoIOBuffers(false)
    design.writeCheckpoint(args(4), CodePerfTracker.SILENT)
    t.stop.printSummary()
  }
}
