package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.XilinxDeviceFamily
import spinal.lib._
import spinal.core._
import spinal.lib.fsm._
import spinal.core.sim._
import spinal.lib.experimental.math.Floating

abstract class FloatingOperator(
    override val operatorName: String,
    override val entityName: String,
    override val params: Seq[(String, Any)],
    convert: Boolean,
    operandNum: Int // 1 or 2
) extends Component with floatingFlopoco{

  assert(Seq(1, 2).contains(operandNum))

  val exponentSize: Int
  val mantissaSize: Int

  val x = in (Floating(exponentSize, mantissaSize))
  val y = (operandNum==2).generate(in (Floating(exponentSize, mantissaSize)))
  val z = out (Floating(exponentSize, mantissaSize))

  val binaryOperand = operandNum==2

  val blackBoxRTLPath = this.verilogFile.getAbsolutePath
  val blackBoxDefinitionName = this.moduleName
  val blackBox = new BlackBox {
    val addtionBits = if(convert) 2 else 0
    val signBit = 1
    val clk = in Bool()
    mapClockDomain(clock = clk)
    val X = in Bits(exponentSize+mantissaSize+signBit+addtionBits bits)
    val Y = binaryOperand.generate(in Bits(exponentSize+mantissaSize+signBit+addtionBits bits))
    val R = out Bits(exponentSize+mantissaSize+signBit+addtionBits bits)
    addRTLPath(blackBoxRTLPath)
    setDefinitionName(blackBoxDefinitionName)
  }

  if(convert) {
    val f2i = Flopoco2Ieee(exponentSize, mantissaSize, family, targetFrequency)
    val i2f0 = Ieee2Flopoco(exponentSize, mantissaSize, family, targetFrequency)
    val i2f1 = binaryOperand.generate(Ieee2Flopoco(exponentSize, mantissaSize, family, targetFrequency))

    i2f0.X := x.asBits
    binaryOperand.generate(i2f1.X := y.asBits)

    blackBox.X := i2f0.R
    binaryOperand.generate(blackBox.Y := i2f1.R)

    f2i.X := blackBox.R
    z.assignFromBits(RegNext(f2i.R))
  }
  else {
    blackBox.X := x.asBits
    binaryOperand.generate(blackBox.Y := y.asBits)
    z.assignFromBits(RegNext(blackBox.R))
  }
}
