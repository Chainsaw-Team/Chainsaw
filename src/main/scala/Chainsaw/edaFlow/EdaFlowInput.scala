package Chainsaw.edaFlow

import Chainsaw.edaFlow.EdaFlowUtils.EdaDirectoryUtils.{genRtlSourcesFromComponent, parseRtlDirFor}
import spinal.core._
import java.io.File
import scala.collection.mutable.ArrayBuffer

abstract class ChainsawEdaFlowInput[T <: Module](
    design: => T,
    val designDirs: Seq[File],
    val workspaceDir: File,
    val topModuleName: String,
    val customizedConfig: Option[SpinalConfig] = None
) {
  def module = design

  def getRtlDir(supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File]
}

class ChainsawEdaModuleInput[T <: Module](
    design: => T,
    workspaceDir: File,
    topModuleName: String,
    customizedConfig: Option[SpinalConfig] = None
) extends ChainsawEdaFlowInput(design, Seq[File](), workspaceDir, topModuleName, customizedConfig) {

  var finishRtlDirGen                 = false
  val rtlResources: ArrayBuffer[File] = ArrayBuffer()

  def getRtlDir(supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File] = {
    if (!finishRtlDirGen) {
      rtlResources ++= genRtlSourcesFromComponent(
        design,
        customizedConfig,
        topModuleName,
        workspaceDir,
        supportedFileTypes
      )
      finishRtlDirGen = true
    }
    rtlResources
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
) extends ChainsawEdaFlowInput(null, Seq[File](), workspaceDir, topModuleName, None) {

  var finishRtlDirGen                 = false
  val rtlResources: ArrayBuffer[File] = ArrayBuffer()

  def getRtlDir(supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File] = {
    if (!finishRtlDirGen) {
      rtlResources ++= parseRtlDirFor(designDirs, supportedFileTypes)
      finishRtlDirGen = true
    }
    rtlResources
  }
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
) extends ChainsawEdaFlowInput(design, designDirs, workspaceDir, topModuleName, customizedConfig) {

  var finishRtlDirGen                 = false
  val rtlResources: ArrayBuffer[File] = ArrayBuffer()

  def getRtlDir(supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File] = {
    if (!finishRtlDirGen) {
      rtlResources ++= parseRtlDirFor(designDirs, supportedFileTypes)
      rtlResources ++= genRtlSourcesFromComponent(
        design,
        customizedConfig,
        topModuleName,
        workspaceDir,
        supportedFileTypes
      )
      finishRtlDirGen = true
    }
    rtlResources
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
