package Chainsaw

object ChainsawDuts {

  // a pass-through
  def simpleDut(correct: Boolean) = {

    val basic = Seq(-1, 0, 1, 2, 3, 4, 5, -1)
    val frame = FrameFormat(basic, 4)
    val formatI = frame.repeat(2).interpolate(2).pad(2)
    val formatO = formatI

    //    println(s"basic = $basic")
    //    println(s"formatI = $formatI")

    val gen: ChainsawGenerator = new ChainsawGenerator {
      override def name = "test"

      override def impl(dataIn: Seq[Any]) = if (correct) dataIn else dataIn.reverse

      override val metric = ChainsawMetric.defaultMetric

      override def inputTypes = Seq.fill(4)(UIntInfo(4))
      override def outputTypes = Seq.fill(4)(UIntInfo(4))

      override def inputFormat = formatI
     override def outputFormat = formatO
      override def latency = 5

      override def implH = new ChainsawModule(this) {
        dataOut := dataIn.d(latency)
      }
    }
    gen
  }
}
