package Chainsaw.deprecated

import Chainsaw.{deprecated, logger, verbose}

// TODO: simplify this
// TODO: comment on this
case class CompressTreeSolution(solutions: Seq[StageSolution]) {
  def getLatency: Int = solutions.count(_.isPipeline)

  def isRedundant = solutions.exists(_.isRedundant)

  def getAreaCostInStage(stage: Int): Double = solutions(stage).getAreaCost

  def getReductionEfficiencyInStage(stage: Int) = solutions(stage).getReductionEfficiency

  def getReductionRatioInStage(stage: Int) = solutions(stage).getReductionRatio

  def getBitReductionInStage(stage: Int) = solutions(stage).getBitReduction

  def getHeightReductionInStage(stage: Int) = solutions(stage).getHeightReduction

  def getStageHeightDiffInStage(stage: Int) = solutions(stage).getStageHeightDiff

  def getWholeHeightDiffInStage(stage: Int) = solutions(stage).getWholeHeightDiff

  def getWidthOutInStage(stage: Int) = solutions(stage).getWidthOut

  def getNextBitHeapInStage(stage: Int) = BitHeaps(solutions(stage).getNextBitHeapConfig: _*)

  def getTotalAreaCost = solutions.map(_.getAreaCost).sum

  def getTotalCompressedBit = solutions.map(_.getBitReduction).sum

  def getTotalEfficiency = getTotalCompressedBit.toDouble / getTotalAreaCost

  def getFinalBitHeap = if (solutions.nonEmpty) new BitHeaps(solutions.last.getNextBitHeapConfig: _*) else null

  def getFinalWidthOut = if (getFinalBitHeap != null) getFinalBitHeap.width + getFinalBitHeap.weightLow else 0

  def printLog(srcBitHeap: BitHeaps[Int]): Unit = {
    solutions.zipWithIndex.foreach { case (stageResolution, stage) =>
      if (verbose >= 1)
        logger.info(
          s"compressed info :\n\tstage bit reduction: ${stageResolution.getBitReduction}, stage reduction efficiency: ${stageResolution.getReductionEfficiency}, stage reduction ratio: ${stageResolution.getReductionRatio}" +
            s"\n\tarea cost: ${stageResolution.getAreaCost}, height: ${stageResolution.getStageHeightDiff}" +
            s"\n\tcompressors used: ${stageResolution.getUsedCompressor.mkString(",")}" +
            s"\n\twhole info :\n\theight: ${stageResolution.getWholeHeightDiff}, bits remained: ${getNextBitHeapInStage(stage).bitsCount}"
        )
      if (stageResolution.isFinalStage && verbose >= 1) logger.info(s"\n${getNextBitHeapInStage(stage).toString}")
    }
    val actualWidth = if (getFinalWidthOut != 0) getFinalBitHeap.widths.max else srcBitHeap.width
    logger.info(
      s"\n----efficiency report of bit heap compressor----" +
        s"\n\tcost in total: $getTotalAreaCost, compressed in total: $getTotalCompressedBit" +
        s"\n\tefficiency in total: $getTotalEfficiency" +
        s"\n\tideal widthOut: ${srcBitHeap.maxValue.bitLength}, actual widthOut: $actualWidth" +
        s"\n\t${if (isRedundant) "has redundant compressor" else "all compressor isn't redundant"}" +
        s"\n\t${if (actualWidth > srcBitHeap.maxValue.bitLength) "output is redundant, need to be resized" else "output isn't redundant"}"
    )
  }
}

case class StageSolution(compressorSolutions: Seq[CompressorSolution], consideration: Consideration, stageInfo: StageInfo) {
  def getUsedCompressor: Set[String] = compressorSolutions.map(_.compressorName).toSet

  def getAreaCost = consideration.areaCost

  def getReductionEfficiency = consideration.reductionEfficiency

  def getReductionRatio = consideration.reductionRatio

  def getBitReduction = consideration.bitReduction

  def getHeightReduction = consideration.heightReduction

  def getStageHeightDiff = stageInfo.stageHeightDiff

  def getWholeHeightDiff = stageInfo.wholeHeightDiff

  def getWidthOut = deprecated.BitHeaps(getNextBitHeapConfig: _*).width

  def getNextBitHeap = deprecated.BitHeaps(getNextBitHeapConfig: _*)

  def getNextBitHeapConfig = stageInfo.nextBitHeapConfig

  def isFinalStage = stageInfo.finalStage

  def isPipeline = stageInfo.isPipeline

  def isRedundant = stageInfo.isRedundant

  def resizeNextHeap(width: Int) = StageSolution(compressorSolutions, consideration, stageInfo.resizeNextHeap(width))
}

case class CompressorSolution(compressorName: String, width: Int, startIndex: Int, consideration: Consideration) {
  def getAreaCost = consideration.areaCost

  def getReductionEfficiency = consideration.reductionEfficiency

  def getReductionRatio = consideration.reductionRatio

  def getBitReduction = consideration.bitReduction

  def getHeightReduction = consideration.heightReduction
}

case class Consideration(areaCost: Double = 0.0, reductionEfficiency: Double = 0, reductionRatio: Double = 1, bitReduction: Int = 0, heightReduction: Int = 1)

case class StageInfo(stageHeightDiff: String, wholeHeightDiff: String, nextBitHeapConfig: Seq[BitHeapConfigInfo[Int]], finalStage: Boolean, isPipeline: Boolean, isRedundant: Boolean) {
  def resizeNextHeap(width: Int) = StageInfo(stageHeightDiff, wholeHeightDiff, nextBitHeapConfig.map(_.resize(width)), finalStage, isPipeline, isRedundant)
}
