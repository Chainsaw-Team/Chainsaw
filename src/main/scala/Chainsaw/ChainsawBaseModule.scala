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

class ChainsawOperatorModule(override val gen: ChainsawOperatorGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._

  validOut := validIn.validAfter(latency)

  setDefinitionName(gen.name)
  setName(gen.name, weak = true)
}

class ChainsawDynamicOperatorModule(override val gen: ChainsawDynamicOperatorGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._

  val controlIn = in Vec controlTypes.map(_.apply())

  setDefinitionName(gen.name)
  setName(gen.name, weak = true)
}

class ChainsawFrameModule(override val gen: ChainsawFrameGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._

  validOut := validIn.validAfter(latency)
  val lastOut = out Bool()

  if (atSimTime) {
    val inputCounter = Counter(period, inc = validIn)
    assert(!(!validIn && inputCounter.value =/= 0), "input frame incomplete")
    val frameCounter = Counter(16384, inc = lastOut)
    frameCounter.value.setName("frameId")
  }

  setDefinitionName(gen.name)
  setName(gen.name, weak = true)
}

class ChainsawDynamicFrameModule(override val gen: ChainsawDynamicFrameGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._

  val lastOut = out Bool()
  val controlIn = in Vec controlTypes.map(_.apply())

  if (atSimTime) {
    val frameCounter = Counter(16384, inc = lastOut)
    frameCounter.value.setName("frameId")
  }

  setDefinitionName(gen.name)
  setName(gen.name, weak = true)
}

class ChainsawInfiniteModule(override val gen: ChainsawInfiniteGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._

  validOut := validIn.validAfter(latency)

  setDefinitionName(gen.name)
  setName(gen.name, weak = true)
}

class ChainsawDynamicInfiniteModule(override val gen: ChainsawDynamicInfiniteGenerator)
  extends ChainsawBaseModule(gen) {

  import gen._


  val controlIn = in Vec controlTypes.map(_.apply())

  setDefinitionName(gen.name)
  setName(gen.name, weak = true)
}
