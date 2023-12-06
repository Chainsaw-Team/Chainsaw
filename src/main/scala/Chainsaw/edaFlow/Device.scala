package Chainsaw.edaFlow

import Chainsaw._
import Chainsaw.edaFlow.vivado.VivadoUtil
import spinal.core._

import java.io.File

object Device {

  sealed trait Vendor
  object Xilinx extends Vendor
  object Altera extends Vendor

  sealed trait Family

  sealed trait XilinxDeviceFamily extends Family
  object UltraScale extends XilinxDeviceFamily
  object UltraScalePlus extends XilinxDeviceFamily
  object Series7 extends XilinxDeviceFamily

  sealed trait AlteraDeviceFamily extends Family

  object CycloneV extends AlteraDeviceFamily

  sealed trait GenericFamily extends Family

  object Generic extends GenericFamily

  case class ChainsawDevice(
      vendor: Vendor,
      deviceFamily: Family,
      familyPart: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) {}

  class XilinxDevice(
      family: XilinxDeviceFamily,
      part: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) extends ChainsawDevice(Xilinx, family, part, fMax, xdcFile, budget) {

    def bramCapacity = budget.bram36 * 36 * 1024 / pow2(20).toDouble

    def uramCapacity = budget.uram288 * 288 * 1024 / pow2(20).toDouble

    def onChipStorage = bramCapacity + uramCapacity

  }

  class AlteraDevice(
      family: AlteraDeviceFamily,
      part: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) extends ChainsawDevice(Altera, family, part, fMax, xdcFile, budget) {

    def bramCapacity = budget.bram36 * 36 * 1024 / pow2(20).toDouble

    def uramCapacity = budget.uram288 * 288 * 1024 / pow2(20).toDouble

    def onChipStorage = bramCapacity + uramCapacity

  }

  class GenericDevice(
      family: GenericFamily,
      part: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) extends ChainsawDevice(Xilinx, family, part, fMax, xdcFile, budget) {

    def bramCapacity = budget.bram36 * 36 * 1024 / pow2(20).toDouble

    def uramCapacity = budget.uram288 * 288 * 1024 / pow2(20).toDouble

    def onChipStorage = bramCapacity + uramCapacity

  }

  val vu9p = new XilinxDevice(
    UltraScale,
    "xcvu9p-flga2104-2-i",
    300 MHz,
    None,
    budget = VivadoUtil(lut = 1182240, ff = 2364480, dsp = 6840, bram36 = 2160, uram288 = 960, carry8 = 147780)
  )
  val zcu104  = new XilinxDevice(UltraScale, "xczu7ev-ffvc1156-2-e", 200 MHz, None)
  val u250    = new XilinxDevice(UltraScale, "XCU250-FIGD2104-2L-E".toLowerCase, 600 MHz, None)
  val u200    = new XilinxDevice(UltraScale, "XCU200-FSGD2104-2-E".toLowerCase, 600 MHz, None, budget = vu9p.budget)
  val kcu1500 = new XilinxDevice(UltraScale, "xcku115-flvb2104-2-e", 800 MHz, None)
  val a7100t = new XilinxDevice(
    Series7,
    "XC7A100T-CSG324-1".toLowerCase(),
    100 MHz,
    Some(new File(xdcFileDir, "NexysA7100T/NexysA7100T-activatedPins.xdc")),
    budget = VivadoUtil(lut = 32600, ff = 65200, dsp = 240)
  )
  val generic = new GenericDevice(Generic, "", 600 MHz, None)

}
