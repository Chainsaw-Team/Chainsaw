package Chainsaw

sealed trait OperatorType

sealed trait AdderType extends OperatorType

object BinaryAdder extends AdderType

object BinarySubtractor extends AdderType

object TernaryAdder extends AdderType

object TernarySubtractor1 extends AdderType

object TernarySubtractor2 extends AdderType

object Compressor extends AdderType

sealed trait MultiplierType extends OperatorType

object FullMultiplier extends MultiplierType

object SquareMultiplier extends MultiplierType

object MsbMultiplier extends MultiplierType

object LsbMultiplier extends MultiplierType

object Kara extends MultiplierType

trait VarType extends OperatorType

object Input extends VarType

object Output extends VarType

object Var extends VarType

sealed trait Strategy

sealed trait TimeDiffStrategy extends Strategy

object IncreaseTimeDiff extends TimeDiffStrategy

object DecreaseTimeDiff extends TimeDiffStrategy

object RandomTimeDiff extends TimeDiffStrategy

object NoneTimeDiff extends TimeDiffStrategy

sealed trait CompressTreeType

object BasicCompressTree extends CompressTreeType

sealed trait ChainsawEnum

trait CpaMode extends ChainsawEnum

object M2M extends CpaMode

object M2S extends CpaMode

object S2M extends CpaMode

object S2S extends CpaMode

sealed trait FilterType extends ChainsawEnum

object Direct1 extends FilterType

object Direct2 extends FilterType

object Transpose1 extends FilterType

object Transpose2 extends FilterType

object Systolic extends FilterType

sealed trait ImplMode extends ChainsawEnum

object FrameBased extends ImplMode

object Infinite extends ImplMode

sealed trait AlgebraicMode extends ChainsawEnum

object CIRCULAR extends AlgebraicMode

object HYPERBOLIC extends AlgebraicMode

object LINEAR extends AlgebraicMode

sealed trait RotationMode extends ChainsawEnum

object ROTATION extends RotationMode

object VECTORING extends RotationMode

sealed trait DdsSignalType extends ChainsawEnum

object SINE extends DdsSignalType

object PULSE extends DdsSignalType

sealed trait EdaFlowType extends ChainsawEnum

object SYNTH extends EdaFlowType

object IMPL extends EdaFlowType

sealed trait CompressionStrategy extends ChainsawEnum
object ReFirst extends CompressionStrategy // ReductionEfficiency First
object HrFirst extends CompressionStrategy // HeightReduction First

sealed trait UtilRequirementStrategy extends ChainsawEnum
object DefaultRequirement
    extends UtilRequirementStrategy // warning when ff < lut*2, don't care about carry8, 5% for tolerant
object PreciseRequirement extends UtilRequirementStrategy // ff and carry8 are considered
object NoRequirement extends UtilRequirementStrategy

sealed trait RamType extends ChainsawEnum

object LUTRAM extends RamType

object BRAM extends RamType

object URAM extends RamType

sealed trait MwNrMode extends ChainsawEnum

object PURELOGIC extends MwNrMode
object REPLICATION extends MwNrMode
object MULTIPUMPING extends MwNrMode
