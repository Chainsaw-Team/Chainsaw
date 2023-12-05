package Chainsaw.edaFlow

import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow.EdaFlowUtils.EdaDirectoryUtils.{genRtlSourcesAndDeviceFrom, parseRtlDirFor}
import spinal.core._

import java.io.File
import scala.collection.mutable.ArrayBuffer

sealed trait ChainsawEdaFlowInput {
  def getWorkspaceDir(): File

  def getTopModuleName(): String

  def getRtlDir(): Seq[File]

  def getDevice(candidate: ChainsawDevice): ChainsawDevice

  def getXdcFile(candidate: File): File

}

class ChainsawEdaModuleInput[T <: Module](
    design: => T,
    workspaceDir: File,
    topModuleName: String,
    customizedConfig: Option[SpinalConfig] = None
) extends ChainsawEdaFlowInput {

  val supportedFileTypes: Seq[String] = Seq(".v", ".sv")
  val (rtlResources, boardDevice, boardXdcFile) = genRtlSourcesAndDeviceFrom(
    design,
    customizedConfig,
    topModuleName,
    workspaceDir,
    supportedFileTypes
  )

  override def getWorkspaceDir(): File = workspaceDir

  override def getTopModuleName(): String = topModuleName

  override def getRtlDir(): Seq[File] = rtlResources

  override def getDevice(candidate: ChainsawDevice = vu9p): ChainsawDevice = {
    boardDevice += candidate
    boardDevice.head
  }

  override def getXdcFile(candidate: File): File = {
    boardXdcFile += candidate
    boardXdcFile.head
  }
}

object ChainsawEdaModuleInput {
  def apply[T <: Module](
      design: => T,
      workspaceDir: File,
      topModuleName: String,
      customizedConfig: Option[SpinalConfig] = None
  ) = new ChainsawEdaModuleInput(design, workspaceDir, topModuleName, customizedConfig)
}

class ChainsawEdaDirInput(
    designDirs: Seq[File],
    workspaceDir: File,
    topModuleName: String
) extends ChainsawEdaFlowInput {

  val supportedFileTypes: Seq[String] = Seq(".v", ".sv")
  val rtlResources                    = parseRtlDirFor(designDirs, supportedFileTypes)

  override def getWorkspaceDir(): File = workspaceDir

  override def getTopModuleName(): String = topModuleName

  override def getRtlDir(): Seq[File] = rtlResources

  override def getDevice(candidate: ChainsawDevice): ChainsawDevice = candidate

  override def getXdcFile(candidate: File): File = candidate
}

object ChainsawEdaDirInput {
  def apply[T <: Module](
      designDirs: Seq[File],
      workspaceDir: File,
      topModuleName: String
  ) = new ChainsawEdaDirInput(designDirs, workspaceDir, topModuleName)
}

class ChainsawEdaFullInput[T <: Module](
    design: => T,
    designDirs: Seq[File],
    workspaceDir: File,
    topModuleName: String,
    customizedConfig: Option[SpinalConfig] = None
) extends ChainsawEdaFlowInput {

  val supportedFileTypes: Seq[String] = Seq(".v", ".sv")
  val (rtlResources, boardDevice, boardXdcFile) = genRtlSourcesAndDeviceFrom(
    design,
    customizedConfig,
    topModuleName,
    workspaceDir,
    supportedFileTypes
  )

  override def getWorkspaceDir(): File = workspaceDir

  override def getTopModuleName(): String = topModuleName

  override def getRtlDir(): Seq[File] = {
    rtlResources ++= parseRtlDirFor(designDirs, supportedFileTypes)
    rtlResources
  }

  override def getDevice(candidate: ChainsawDevice): ChainsawDevice = {
    boardDevice += candidate
    boardDevice.head
  }

  override def getXdcFile(candidate: File): File = {
    boardXdcFile += candidate
    boardXdcFile.head
  }
}

object ChainsawEdaFullInput {
  def apply[T <: Module](
      design: => T,
      designDirs: Seq[File],
      workspaceDir: File,
      topModuleName: String,
      customizedConfig: Option[SpinalConfig] = None
  ) = new ChainsawEdaFullInput(design, designDirs, workspaceDir, topModuleName, customizedConfig)
}
