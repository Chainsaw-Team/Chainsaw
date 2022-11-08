package Chainsaw.xilinx

import spinal.core.HertzNumber

import java.io.File

/**
 * @param family  device family
 * @param part    full par name
 * @param fMax    recommended fmax
 * @param xdcFile xdc file for specific board
 */
case class XilinxDevice(
                         family: XilinxDeviceFamily,
                         part: String,
                         fMax: HertzNumber,
                         xdcFile: Option[File]
                       )
