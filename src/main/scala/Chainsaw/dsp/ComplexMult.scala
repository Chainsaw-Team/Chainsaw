package Chainsaw.dsp

import Chainsaw.ChainsawMetric.{complexBound, forallBound}
import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.xilinx.VivadoUtilRequirement
import breeze.math.Complex
import spinal.core._

import scala.reflect.runtime.universe

case class ComplexMult(dataType: NumericType, coeffType: NumericType, productType: NumericType) extends ChainsawGenerator {

  override def name = getAutoName(this)

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[Complex]].product)

  override def generateTestCases = Seq.fill(1000)(Seq(dataType.getRandom, coeffType.getRandom)).flatten

  override val metric = ChainsawMetric(frameWise = forallBound(complexBound(productType.resolution * 10)))

  override def inputTypes = Seq(dataType.toComplexFixInfo, coeffType.toComplexFixInfo)

  override def outputTypes = Seq(productType.toComplexFixInfo)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  override def latency = 6

  override def utilEstimation = VivadoUtilRequirement(dsp = 3, lut = 10)

  override def implH = new ChainsawModule(this) {

    val Seq(data, coeff) = complexDataIn
    val (ar, ai) = (data.real, data.imag)
    val (br, bi) = (coeff.real, coeff.imag)

    // regs outside dsp
    val arD1 = ar.d()
    val aiD2 = ai.d(2)
    val brD2 = br.d(2)
    val biD2 = bi.d(2)
    // dsp operation and regs inside dsp
    val mid = ((br.d() +^ bi.d()).d() * ar.d(2)).d(2)
    val productImag = (mid.d() + ((aiD2.d() -^ arD1.d(2)).d() * brD2.d(2)).d()).d()
    val productReal = (mid.d() - ((aiD2.d() +^ arD1.d(2)).d() * biD2.d(2)).d()).d()
    complexDataOut.head := ComplexFix(productReal, productImag).truncate(productType.toComplexFixInfo)
  }
}

object ComplexMult {
  def apply(dataType: NumericType, coeffType: NumericType): ComplexMult = new ComplexMult(dataType, coeffType, dataType)

  def apply(dataType: NumericType): ComplexMult = new ComplexMult(dataType, dataType, dataType)
}


