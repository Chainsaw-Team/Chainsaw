package Chainsaw


import spinal.core._
import spinal.lib._

abstract class ChainsawBaseModule(val gen: ChainsawBaseGenerator) extends Component {

  import gen._

  val flowIn = slave Flow Vec(inputTypes.map(_.apply()))
  val flowOut = master Flow Vec(outputTypes.map(_.apply()))

  val dataIn = flowIn.payload
  val validIn = flowIn.valid

  val dataOut = flowOut.payload
  val validOut = flowOut.valid

  setDefinitionName(gen.name)
  setName(gen.name, weak = true)
}

trait DynamicModule {
  val controlIn: Vec[AFix]
}

class ChainsawOperatorModule(override val gen: ChainsawOperatorGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._

  validOut := validIn.validAfter(latency())
}

class ChainsawDynamicOperatorModule(override val gen: ChainsawDynamicOperatorGenerator)
  extends ChainsawBaseModule(gen) with DynamicModule {

  import gen._

  override val controlIn = in Vec controlTypes.map(_.apply())
}

class ChainsawFrameModule(override val gen: ChainsawFrameGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._

  if (!isInstanceOf[DynamicModule]) {
    validOut := validIn.validAfter(latency())
  }
  val lastOut = out Bool()
  if (atSimTime) {
    if (!isInstanceOf[DynamicModule]) {
      val inputCounter = Counter(period, inc = validIn)
      assert(!(!validIn && inputCounter.value =/= 0), "input frame incomplete")
    }
    val frameCounter = Counter(16384, inc = lastOut)
    frameCounter.value.setName("frameId")
  }
}

class ChainsawDynamicFrameModule(override val gen: ChainsawDynamicFrameGenerator)
  extends ChainsawBaseModule(gen) with DynamicModule {

  import gen._

  override val controlIn = in Vec controlTypes.map(_.apply())
  val lastOut = out Bool()
}

class ChainsawInfiniteModule(override val gen: ChainsawInfiniteGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._

  validOut := validIn.validAfter(latency())
}

class ChainsawDynamicInfiniteModule(override val gen: ChainsawDynamicInfiniteGenerator)
  extends ChainsawBaseModule(gen) with DynamicModule {

  import gen._

  override val controlIn = in Vec controlTypes.map(_.apply())
}