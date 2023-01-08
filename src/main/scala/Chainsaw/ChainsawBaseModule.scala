package Chainsaw

import spinal.core._
import spinal.lib._

abstract class ChainsawBaseModule(val gen: ChainsawBaseGenerator)
    extends Component {

  import gen._

  val flowIn  = slave Flow Fragment(Vec(inputTypes.map(_.apply())))
  val flowOut = master Flow Fragment(Vec(outputTypes.map(_.apply())))

  val dataIn  = flowIn.fragment
  val validIn = flowIn.valid
  val lastIn  = flowIn.last

  val dataOut  = flowOut.fragment
  val validOut = flowOut.valid
  val lastOut  = flowOut.last

  def dataIo = dataIn.map(_.raw) ++ dataOut.map(_.raw)

  // these pointers can be modified by elaboration phases
  var flowInPointer  = flowIn
  var flowOutPointer = flowOut

  gen match {
    case fixedLatency: FixedLatency =>
      validOut := validIn.validAfter(fixedLatency.latency())
    case _ =>
  }

  if (atSimTime) {
    val segmentCounter = Counter(16384, inc = lastOut)
    segmentCounter.value.setName("segmentId")
  }

  // SpinalHDL will generate multiple copies of same module when multiple modules contains a same ROM file

  // FIXME: "already used once for a different layout", when multiple modules contains a same ROM file
  //  setDefinitionName(gen.name)
  setName(gen.name, weak = true)

  /** -------- connection utils
    * --------
    */
  def map(func: Seq[AFix] => Seq[AFix], latency: Int = 0): ChainsawFlow =
    flowOut.mapFragment(func, latency)

  def fixTo(af: AFix) = map(_.map(_.fixTo(af)))
}

trait DynamicModule {
  val controlIn: Vec[AFix]
}

class ChainsawOperatorModule(override val gen: ChainsawOperatorGenerator)
    extends ChainsawBaseModule(gen) {
  lastOut := validOut
}

class ChainsawDynamicOperatorModule(
    override val gen: ChainsawDynamicOperatorGenerator
) extends ChainsawBaseModule(gen)
    with DynamicModule {

  import gen._

  override val controlIn = in Vec controlTypes.map(_.apply())
  lastOut := validOut
}

class ChainsawFrameModule(override val gen: ChainsawFrameGenerator)
    extends ChainsawBaseModule(gen) {

  import gen._

  if (atSimTime) {
    val inputCounter = Counter(period, inc = validIn)
    assert(!(!validIn && inputCounter.value =/= 0), "input frame incomplete")
  }
}

class ChainsawDynamicFrameModule(
    override val gen: ChainsawDynamicFrameGenerator
) extends ChainsawBaseModule(gen)
    with DynamicModule {

  import gen._

  override val controlIn = in Vec controlTypes.map(_.apply())
}

class ChainsawInfiniteModule(override val gen: ChainsawInfiniteGenerator)
    extends ChainsawBaseModule(gen) {
  lastOut.assignDontCare()
}

class ChainsawDynamicInfiniteModule(
    override val gen: ChainsawDynamicInfiniteGenerator
) extends ChainsawBaseModule(gen)
    with DynamicModule {

  import gen._

  override val controlIn = in Vec controlTypes.map(_.apply())
  lastOut.assignDontCare()
}

abstract class ChainsawCustomModule extends Component {

  def inputTypes: Seq[NumericType]

  def outputTypes: Seq[NumericType]

  val flowIn  = slave Flow Fragment(Vec(inputTypes.map(_.apply())))
  val flowOut = master Flow Fragment(Vec(outputTypes.map(_.apply())))

  val dataIn  = flowIn.fragment
  val validIn = flowIn.valid
  val lastIn  = flowIn.last

  val dataOut  = flowOut.fragment
  val validOut = flowOut.valid
  val lastOut  = flowOut.last
}
