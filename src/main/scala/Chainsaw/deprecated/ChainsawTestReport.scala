package Chainsaw.deprecated

// TODO: a richer testReport
case class ChainsawTestReport(success: Boolean, input: Seq[Any], yours: Seq[Any], golden: Seq[Any])
