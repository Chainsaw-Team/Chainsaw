package Chainsaw

import Chainsaw.edaFlow.Device._
import spinal.core._
import spinal.core.internals.PhaseContext

import java.io.File

package object edaFlow {

  trait EdaOptimizeOption

  trait Report

  val xdcFileDir = new File("src/main/resources/xdc")

  def xilinxDefaultCDConfig = ClockDomainConfig( // recommended by Xilinx UG901
    clockEdge              = RISING,
    resetKind              = ASYNC,
    resetActiveLevel       = HIGH,
    softResetActiveLevel   = HIGH,
    clockEnableActiveLevel = HIGH
  )

  def xilinxDefaultSpinalConfig = SpinalConfig(mode = Verilog, defaultConfigForClockDomains = xilinxDefaultCDConfig)

  /** general interface for edaFlow
    * @param designDirs
    *   the source verilog or systemVerilog file of design will be used in edaFlow
    * @param workspaceDir
    *   the father directory of script and rtl file which generated by edaFlow
    * @param topModuleName
    *   the topModuleName of all module, the validity of the topModuleName is guaranteed by the designer
    * @param device
    *   the device which will be used in edaFlow to synthesis or implementation
    * @param taskType
    *   specify the task type (Simulation, Synthesis, Implementation, Generate Binary)
    * @param optimizeOption
    *   specify the optimize option will be used in edaFlow
    * @param blackBoxSet
    *   specify which module will be viewed as BlackBox(will change v or sv file)
    */
  abstract class EdaFlow(
      designDirs: Seq[File],
      workspaceDir: File,
      topModuleName: String,
      device: ChainsawDevice,
      taskType: EdaFlowType,
      optimizeOption: EdaOptimizeOption,
      blackBoxSet: Option[Set[String]]
  ) {

    def genScript(): String

    def runScript(): Unit

  }

  def inVirtualGlob[T](func: => T): T = {
    val old = GlobalData.get

    val virtualGlob = new GlobalData(SpinalConfig())
    virtualGlob.phaseContext = new PhaseContext(SpinalConfig())
    GlobalData.set(virtualGlob)
    val ret = func

    GlobalData.set(old)
    ret
  }

}
