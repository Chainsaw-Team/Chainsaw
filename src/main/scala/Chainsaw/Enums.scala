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

object Custom extends OperatorType

object Multiplexer extends OperatorType

object And extends OperatorType

object Shift extends OperatorType

object Split extends OperatorType

object Merge extends OperatorType

object Resize extends OperatorType

sealed trait SopStructure

object Direct extends SopStructure

object Transpose extends SopStructure

object Systolic extends SopStructure