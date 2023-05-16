package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.arithmetic.bitheap.Bit
import Chainsaw.arithmetic.flopoco.{FlopocoBlackBox, XilinxGpc}
import Chainsaw.device.LUT5to2.toLUT5_2Format
import Chainsaw.device.{CARRY8, LUT5to2, LUT6_2, LUTUtils}
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim.SpinalSimBackendSel.GHDL
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.math.BigInt

/** the abstract class of general parallel compressor(gpc) which define almost all information need, but some
  * information needs to be defined in the subclass according to the specific compressor
  */
abstract class Gpc extends CompressorGenerator {

  /** this method is used to get the name of gpc
    * @return
    *   the name of gpc
    */
  def name =
    s"${className(this)}_${if (shouldDoComplement) hashName(getComplementHeap)
    else "noComplement"}"

  // column in
  override def inputTypes =
    inputFormat.filter(_ > 0).map(NumericType.U)

  // row out
  override def outputTypes = {
    require(outputFormat.max == 1)
    Seq(NumericType.U(outputFormat.length))
  }

  val inputWeights = inputFormat.zipWithIndex.filter(_._1 > 0).map(_._2)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    yours.head.toBigInt() == golden.head.toBigInt()
  }

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    val ret = dataIn
      .zip(inputWeights)
      .map { case (bits, weight) =>
        bits.asBits.asBools.map(_.asUInt).reduce(_ +^ _) << weight
      }
      .reduce(_ +^ _)
    dataOut.head := ret.toAFix.truncated
  })
}

/** -------- built by Xilinx primitives
  * --------
  */

case class Compressor6to3(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq.fill(1)(6)

  override def outputFormat = Seq.fill(3)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 3, ff = outputFormat.sum)

  override def implH = new ChainsawOperatorModule(this) {
    val lutValues = Seq(
      BigInt("6996966996696996", 16),
      BigInt("8117177e177e7ee8", 16),
      BigInt("fee8e880e8808000", 16)
    )
    val inverseList = getComplementHeap.head.padTo(6, true).map(!_)
    val dataBitsIn  = dataIn.head.raw.asBools
    val lutOuts = lutValues
      .map(LUTUtils.getValueWithInverse(_, inverseList))
      .map(LUT6_2.process(dataBitsIn, _).last)
    dataOut.head := lutOuts.asBits().asUInt.toAFix
  }
}

case class Compressor15to3(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {
  override def inputFormat: Seq[Int] = Seq(5, 1)

  override def outputFormat: Seq[Int] = Seq.fill(3)(1)

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(lut = 3, ff = outputFormat.sum)

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val lutValues = Seq(
      LUTUtils.expression2value(exp => exp.count(_ == true) % 2 == 1, 6),
      LUTUtils.expression2value(
        exp =>
          (exp.zipWithIndex.map { case (bool, i) =>
            if (i == 0 && bool) 2 else if (i != 0 && bool) 1 else 0
          }.sum >> 1) % 2 == 1,
        6
      ),
      LUTUtils.expression2value(
        exp =>
          (exp.zipWithIndex.map { case (bool, i) =>
            if (i == 0 && bool) 2 else if (i != 0 && bool) 1 else 0
          }.sum >> 2) % 2 == 1,
        6
      )
    )
    val inverseList = Seq(
      getComplementHeap.head.padTo(6, true).map(!_),
      (getComplementHeap.last.padTo(1, true) ++ getComplementHeap.head.padTo(5, true)).map(!_),
      (getComplementHeap.last.padTo(1, true) ++ getComplementHeap.head.padTo(5, true)).map(!_)
    )
    val Seq(right, left) = dataIn.map(_.raw.asBools)
    val lutOuts = lutValues
      .zip(inverseList)
      .map { case (value, inverse) =>
        LUTUtils.getValueWithInverse(value, inverse)
      }
      .zipWithIndex
      .map { case (value, i) =>
        i match {
          case 0 => LUT6_2.process(right :+ False, value).last
          case 1 => LUT6_2.process(left ++ right, value).last
          case _ => LUT6_2.process(left ++ right, value).last
        }
      }
    dataOut.head := lutOuts.asBits().asUInt.toAFix
  }
}

case class Compressor14to3(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {
  override def inputFormat: Seq[Int] = Seq(4, 1)

  override def outputFormat: Seq[Int] = Seq.fill(3)(1)

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(lut = 2, ff = outputFormat.sum)

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val inverseList = getComplementHeap.last.padTo(1, true).map(!_) ++ getComplementHeap.head.padTo(4, true).map(!_)

    val lut5to2s = Seq(
      LUT5to2(
        toLUT5_2Format(bits =>
          (bits
            .zip(inverseList)
            .zipWithIndex
            .map { case ((bit, inverse), i) => if (i == 0 && bit ^ inverse) 2 else if (bit ^ inverse) 1 else 0 }
            .sum >> 2) % 2 == 1
        ),
        toLUT5_2Format(bits =>
          (bits
            .zip(inverseList)
            .zipWithIndex
            .map { case ((bit, inverse), i) => if (i == 0 && bit ^ inverse) 2 else if (bit ^ inverse) 1 else 0 }
            .sum >> 2) % 2 == 1
        )
      ),
      LUT5to2(
        toLUT5_2Format(bits =>
          (bits
            .zip(inverseList)
            .zipWithIndex
            .map { case ((bit, inverse), i) => if (i == 0 && bit ^ inverse) 2 else if (bit ^ inverse) 1 else 0 }
            .sum >> 1) % 2 == 1
        ),
        toLUT5_2Format(bits =>
          bits
            .zip(inverseList)
            .zipWithIndex
            .map { case ((bit, inverse), i) => if (i == 0 && bit ^ inverse) 2 else if (bit ^ inverse) 1 else 0 }
            .sum % 2 == 1
        )
      )
    )

    val Seq(right, left) = dataIn.map(_.raw.asBools)
    val lutOuts = lut5to2s
      .flatMap(lut => lut.process(left ++ right: _*))
      .tail
    dataOut.head := lutOuts.reverse.asBits().asUInt.toAFix
  }
}

case class Compressor23to3(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {
  override def inputFormat: Seq[Int] = Seq(3, 2)

  override def outputFormat: Seq[Int] = Seq.fill(3)(1)

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(lut = 2, ff = outputFormat.sum)

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val inverseList = getComplementHeap.last.padTo(2, true).map(!_) ++ getComplementHeap.head.padTo(3, true).map(!_)

    val lut5to2s = Seq(
      LUT5to2(
        toLUT5_2Format(bits =>
          (bits
            .zip(inverseList)
            .zipWithIndex
            .map { case ((bit, inverse), i) => if (i <= 1 && bit ^ inverse) 2 else if (bit ^ inverse) 1 else 0 }
            .sum >> 2) % 2 == 1
        ),
        toLUT5_2Format(bits =>
          (bits
            .zip(inverseList)
            .zipWithIndex
            .map { case ((bit, inverse), i) => if (i <= 1 && bit ^ inverse) 2 else if (bit ^ inverse) 1 else 0 }
            .sum >> 2) % 2 == 1
        )
      ),
      LUT5to2(
        toLUT5_2Format(bits =>
          (bits
            .zip(inverseList)
            .zipWithIndex
            .map { case ((bit, inverse), i) => if (i <= 1 && bit ^ inverse) 2 else if (bit ^ inverse) 1 else 0 }
            .sum >> 1) % 2 == 1
        ),
        toLUT5_2Format(bits =>
          bits
            .zip(inverseList)
            .zipWithIndex
            .map { case ((bit, inverse), i) => if (i <= 1 && bit ^ inverse) 2 else if (bit ^ inverse) 1 else 0 }
            .sum % 2 == 1
        )
      )
    )

    val Seq(right, left) = dataIn.map(_.raw.asBools)
    val lutOuts = lut5to2s
      .flatMap(lut => lut.process(left ++ right: _*))
      .tail
    dataOut.head := lutOuts.reverse.asBits().asUInt.toAFix
  }
}

case class Compressor5to3(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {
  override def inputFormat: Seq[Int] = Seq(5)

  override def outputFormat: Seq[Int] = Seq.fill(3)(1)

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(lut = 2, ff = outputFormat.sum)

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val inverseList = getComplementHeap.head.padTo(5, true).map(!_)

    val lut5to2s = Seq(
      LUT5to2(
        toLUT5_2Format(bits => (bits.zip(inverseList).count { case (bit, inverse) => bit ^ inverse } >> 2) % 2 == 1),
        toLUT5_2Format(bits => (bits.zip(inverseList).count { case (bit, inverse) => bit ^ inverse } >> 2) % 2 == 1)
      ),
      LUT5to2(
        toLUT5_2Format(bits => (bits.zip(inverseList).count { case (bit, inverse) => bit ^ inverse } >> 1) % 2 == 1),
        toLUT5_2Format(bits => bits.zip(inverseList).count { case (bit, inverse) => bit ^ inverse } % 2 == 1)
      )
    )

    val dataBitsIn = dataIn.head.raw.asBools
    val lutOuts    = lut5to2s.flatMap(_.process(dataBitsIn.padTo(5, False): _*)).tail
    dataOut.head := lutOuts.reverse.asBits().asUInt.toAFix
  }
}

case class Compressor3to2(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq.fill(1)(3)

  override def outputFormat = Seq.fill(2)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 1, ff = outputFormat.sum)

  override def implH = new ChainsawOperatorModule(this) {
    val lutValues = BigInt("96969696e8e8e8e8", 16)
    val inverseList =
      getComplementHeap.head.padTo(3, true).map(!_) ++ Seq.fill(3)(false)
    val dataBitsIn    = dataIn.head.raw.asBools // low to high
    val inversedValue = LUTUtils.getValueWithInverse(lutValues, inverseList)
    val lutOuts =
      LUT6_2.process(dataBitsIn ++ Seq(False, False, True), inversedValue)
    dataOut.head := lutOuts.reverse.asBits().asUInt.toAFix
  }
}

case class Compressor1415to5(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 1, 4, 1)

  override def outputFormat = Seq.fill(5)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 4, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(2).take(4).padTo(6, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap.last.take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(2).take(4).padTo(6, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i % 2 == 0)
          LUT6_2.process(
            bitIns,
            LUTUtils
              .getValueWithInverse(
                LUTUtils.expression2value(
                  bits => bits.take(4).count(_ == true) % 2 == 1,
                  6
                ),
                invList
              )
          )
        else
          LUT6_2.process(
            bitIns,
            LUTUtils
              .getValueWithInverse(
                LUTUtils.expression2value(
                  bits => bits(3) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))),
                  6
                ),
                invList
              )
          )
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data =
      (0 until 4).map(i => if (inverseList(i).take(4).last) ~lutBitsIns(i).take(4).last else lutBitsIns(i).take(4).last)

    (0 until 8).foreach { j =>
      if (j < 4) {
        carryChain.DI(j) := data(j)
        carryChain.S(j)  := selects(j)
      } else {
        carryChain.DI(j) := False
        carryChain.S(j)  := False
      }
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(6, true).take(5).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(3) ## carryChain.O.takeLow(4)).asUInt.toAFix
  }
}

case class Compressor1406to5(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(6, 0, 4, 1)

  override def outputFormat = Seq.fill(5)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 4, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(5).padTo(6, true),
      getComplementHeap.head.take(4).padTo(5, true),
      getComplementHeap(2).take(4).padTo(6, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap.last.take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(5).padTo(6, True),
      dataBitsIn.head.take(4).padTo(5, True),
      dataBitsIn(1).take(4).padTo(6, True),
      dataBitsIn(1).take(3).padTo(3, True) ++ dataBitsIn.last.take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        i match {
          case 0 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(5).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          case 1 =>
            LUT5to2(
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                  invList
                )
              ),
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits =>
                    ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^
                      ((bits
                        .take(3)
                        .count(_ == true) % 2 == 1) && bits(3)),
                  invList
                )
              )
            ).process(bitIns: _*)
          case 2 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          case _ =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                    6
                  ),
                  invList
                )
            )
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList(0).take(5).last) ~lutBitsIns(0).take(5).last else lutBitsIns(0).take(5).last,
      lutOuts(1)._1,
      if (inverseList(2).take(4).last) ~lutBitsIns(2).take(4).last else lutBitsIns(2).take(4).last,
      if (inverseList(3).take(4).last) ~lutBitsIns(3).take(4).last else lutBitsIns(3).take(4).last
    )

    (0 until 8).foreach { j =>
      if (j < 4) {
        carryChain.DI(j) := data(j)
        carryChain.S(j)  := selects(j)
      } else {
        carryChain.DI(j) := False
        carryChain.S(j)  := False
      }
    }
    val cIn =
      if (getComplementHeap.head.take(6).padTo(6, true).last) dataBitsIn.head.take(6).padTo(6, False).last
      else ~dataBitsIn.head.take(6).padTo(6, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(3) ## carryChain.O.takeLow(4)).asUInt.toAFix
  }
}

case class Compressor1325to5(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 2, 3, 1)

  override def outputFormat = Seq.fill(5)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 4, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(2).padTo(2, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(1).take(2).padTo(2, true) :+ true,
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(2).padTo(2, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(1).take(2).padTo(2, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn.last.take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        i match {
          case 0 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          case 1 =>
            LUT5to2(
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => bits(3) ^ bits(4),
                  invList
                )
              ),
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3) ^ bits(4),
                  invList
                )
              )
            ).process(bitIns: _*)
          case 2 =>
            LUT5to2(
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => bits(3) && bits(4),
                  invList
                )
              ),
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => (bits.take(3).count(_ == true) % 2 == 1) ^ (bits(3) && bits(4)),
                  invList
                )
              )
            ).process(bitIns: _*)
          case _ =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                    6
                  ),
                  invList
                )
            )
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList(0).take(4).last) ~lutBitsIns(0).take(4).last else lutBitsIns(0).take(4).last,
      lutOuts(1)._1,
      lutOuts(2)._1,
      if (inverseList(3).take(4).last) ~lutBitsIns(3).take(4).last else lutBitsIns(3).take(4).last
    )

    (0 until 8).foreach { j =>
      if (j < 4) {
        carryChain.DI(j) := data(j)
        carryChain.S(j)  := selects(j)
      } else {
        carryChain.DI(j) := False
        carryChain.S(j)  := False
      }
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(3) ## carryChain.O.takeLow(4)).asUInt.toAFix
  }
}

case class Compressor623to5(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(3, 2, 6)

  override def outputFormat = Seq.fill(5)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 4, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(2).padTo(6, true),
      getComplementHeap(1).take(2).padTo(6, true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(2).padTo(6, True),
      dataBitsIn(1).take(2).padTo(6, True),
      dataBitsIn(2).take(6).padTo(6, True),
      dataBitsIn(2).take(4).padTo(4, True) :+ dataBitsIn(2).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        i match {
          case 0 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(2).reduce(_ ^ _),
                    6
                  ),
                  invList
                )
            )
          case 1 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(2).reduce(_ ^ _),
                    6
                  ),
                  invList
                )
            )
          case 2 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(6).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          case _ =>
            LUT5to2(
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                  invList
                )
              ),
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits =>
                    ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(
                      1
                    ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                  invList
                )
              )
            ).process(bitIns: _*)
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(2).last) ~lutBitsIns.head.take(2).last else lutBitsIns.head.take(2).last,
      if (inverseList(1).take(2).last) ~lutBitsIns(1).take(2).last else lutBitsIns(1).take(2).last,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      if (j < 4) {
        carryChain.DI(j) := data(j)
        carryChain.S(j)  := selects(j)
      } else {
        carryChain.DI(j) := False
        carryChain.S(j)  := False
      }
    }
    val cIn =
      if (getComplementHeap.head.take(3).padTo(3, true).last) dataBitsIn.head.take(3).padTo(3, False).last
      else ~dataBitsIn.head.take(3).padTo(3, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(3) ## carryChain.O.takeLow(4)).asUInt.toAFix
  }
}

case class Compressor606to5(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(6, 0, 6)

  override def outputFormat = Seq.fill(5)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 4, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(5).padTo(6, true),
      getComplementHeap.head.take(4).padTo(5, true),
      getComplementHeap.last.take(6).padTo(6, true),
      getComplementHeap.last.take(4).padTo(4, true) :+ getComplementHeap.last.padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(5).padTo(6, True),
      dataBitsIn.head.take(4).padTo(5, True),
      dataBitsIn.last.take(6).padTo(6, True),
      dataBitsIn.last.take(4).padTo(4, True) :+ dataBitsIn.last.padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        i match {
          case 0 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(5).reduce(_ ^ _),
                    6
                  ),
                  invList
                )
            )
          case 1 =>
            LUT5to2(
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                  invList
                )
              ),
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits =>
                    ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ (bits
                      .take(3)
                      .reduce(_ ^ _) && bits(3)),
                  invList
                )
              )
            ).process(bitIns: _*)
          case 2 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(6).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          case _ =>
            LUT5to2(
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                  invList
                )
              ),
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits =>
                    ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(
                      1
                    ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                  invList
                )
              )
            ).process(bitIns: _*)
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(5).last) ~lutBitsIns.head.take(5).last else lutBitsIns.head.take(5).last,
      lutOuts(1)._1,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      if (j < 4) {
        carryChain.DI(j) := data(j)
        carryChain.S(j)  := selects(j)
      } else {
        carryChain.DI(j) := False
        carryChain.S(j)  := False
      }
    }
    val cIn =
      if (getComplementHeap.head.take(6).padTo(6, true).last) dataBitsIn.head.take(6).padTo(6, False).last
      else ~dataBitsIn.head.take(6).padTo(6, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(3) ## carryChain.O.takeLow(4)).asUInt.toAFix
  }
}

case class Compressor615to5(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 1, 6)

  override def outputFormat = Seq.fill(5)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 4, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap.last.take(6).padTo(6, true),
      getComplementHeap.last.take(4).padTo(4, true) :+ getComplementHeap.last.padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn.last.take(6).padTo(6, True),
      dataBitsIn.last.take(4).padTo(4, True) :+ dataBitsIn.last.padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        i match {
          case 0 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).reduce(_ ^ _),
                    6
                  ),
                  invList
                )
            )
          case 1 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                    6
                  ),
                  invList
                )
            )
          case 2 =>
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(6).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          case _ =>
            LUT5to2(
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                  invList
                )
              ),
              toLUT5_2Format(
                LUTUtils.getExpressionWithInverse(
                  bits =>
                    ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(
                      1
                    ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                  invList
                )
              )
            ).process(bitIns: _*)
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(4).last) ~lutBitsIns.head.take(4).last else lutBitsIns.head.take(4).last,
      if (inverseList(1).take(4).last) ~lutBitsIns(1).take(4).last else lutBitsIns(1).take(4).last,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      if (j < 4) {
        carryChain.DI(j) := data(j)
        carryChain.S(j)  := selects(j)
      } else {
        carryChain.DI(j) := False
        carryChain.S(j)  := False
      }
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(3) ## carryChain.O.takeLow(4)).asUInt.toAFix
  }
}

case class Compressor14051415to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 1, 4, 1, 5, 0, 4, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(2).take(4).padTo(6, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(5).padTo(6, true),
      getComplementHeap(4).take(4).padTo(5, true),
      getComplementHeap(6).take(4).padTo(6, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap.last.take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(2).take(4).padTo(6, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(4).take(5).padTo(6, True),
      dataBitsIn(4).take(4).padTo(5, True),
      dataBitsIn(5).take(4).padTo(6, True),
      dataBitsIn(5).take(3).padTo(3, True) ++ dataBitsIn.last.take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          if (i % 2 == 0)
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          else
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits(3) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))),
                    6
                  ),
                  invList
                )
            )
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^
                        ((bits
                          .take(3)
                          .count(_ == true) % 2 == 1) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data =
      (0 until 4).map(i =>
        if (inverseList(i).take(4).last) ~lutBitsIns(i).take(4).last else lutBitsIns(i).take(4).last
      ) ++
        Seq(
          if (inverseList(4).take(5).last) ~lutBitsIns(4).take(5).last else lutBitsIns(4).take(5).last,
          lutOuts(5)._1,
          if (inverseList(6).take(4).last) ~lutBitsIns(6).take(4).last else lutBitsIns(6).take(4).last,
          if (inverseList(7).take(4).last) ~lutBitsIns(7).take(4).last else lutBitsIns(7).take(4).last
        )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor13241415to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 1, 4, 1, 4, 2, 3, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(2).take(4).padTo(6, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(2).padTo(2, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(5).take(2).padTo(2, true) :+ true,
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(7).take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(2).take(4).padTo(6, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(4).take(4).padTo(6, True),
      dataBitsIn(4).take(3).padTo(3, True) ++ dataBitsIn(5).take(2).padTo(2, True),
      dataBitsIn(6).take(3).padTo(3, True) ++ dataBitsIn(5).take(2).padTo(2, True),
      dataBitsIn(6).take(3).padTo(3, True) ++ dataBitsIn(7).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          if (i % 2 == 0)
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          else
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits(3) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))),
                    6
                  ),
                  invList
                )
            )
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) ^ bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3) ^ bits(4),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) && bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.take(3).count(_ == true) % 2 == 1) ^ (bits(3) && bits(4)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data =
      (0 until 4).map(i =>
        if (inverseList(i).take(4).last) ~lutBitsIns(i).take(4).last else lutBitsIns(i).take(4).last
      ) ++
        Seq(
          if (inverseList(4).take(4).last) ~lutBitsIns(4).take(4).last else lutBitsIns(4).take(4).last,
          lutOuts(5)._1,
          lutOuts(6)._1,
          if (inverseList(7).take(4).last) ~lutBitsIns(7).take(4).last else lutBitsIns(7).take(4).last
        )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06051415to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 1, 4, 1, 5, 0, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(2).take(4).padTo(6, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(5).padTo(6, true),
      getComplementHeap(4).take(4).padTo(5, true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(2).take(4).padTo(6, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(4).take(5).padTo(6, True),
      dataBitsIn(4).take(4).padTo(5, True),
      dataBitsIn.last.take(6).padTo(6, True),
      dataBitsIn.last.take(4).padTo(4, True) :+ dataBitsIn.last.padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          if (i % 2 == 0)
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          else
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits(3) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))),
                    6
                  ),
                  invList
                )
            )
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ (bits
                        .take(3)
                        .reduce(_ ^ _) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data =
      (0 until 4).map(i =>
        if (inverseList(i).take(4).last) ~lutBitsIns(i).take(4).last else lutBitsIns(i).take(4).last
      ) ++
        Seq(
          if (inverseList(4).take(5).last) ~lutBitsIns(4).take(5).last else lutBitsIns(4).take(5).last,
          lutOuts(5)._1,
          if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
          lutOuts.last._1
        )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor14050623to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(3, 2, 6, 0, 5, 0, 4, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(2).padTo(6, true),
      getComplementHeap(1).take(2).padTo(6, true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(5).padTo(6, true),
      getComplementHeap(4).take(4).padTo(5, true),
      getComplementHeap(6).take(4).padTo(6, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap.last.take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(2).padTo(6, True),
      dataBitsIn(1).take(2).padTo(6, True),
      dataBitsIn(2).take(6).padTo(6, True),
      dataBitsIn(2).take(4).padTo(4, True) :+ dataBitsIn(2).padTo(6, True).last,
      dataBitsIn(3).take(5).padTo(6, True),
      dataBitsIn(3).take(4).padTo(5, True),
      dataBitsIn(4).take(4).padTo(6, True),
      dataBitsIn(4).take(3).padTo(3, True) ++ dataBitsIn(5).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^
                        ((bits
                          .take(3)
                          .count(_ == true) % 2 == 1) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(2).last) ~lutBitsIns.head.take(2).last else lutBitsIns.head.take(2).last,
      if (inverseList(1).take(2).last) ~lutBitsIns(1).take(2).last else lutBitsIns(1).take(2).last,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1,
      if (inverseList(4).take(5).last) ~lutBitsIns(4).take(5).last else lutBitsIns(4).take(5).last,
      lutOuts(5)._1,
      if (inverseList(6).take(4).last) ~lutBitsIns(6).take(4).last else lutBitsIns(6).take(4).last,
      if (inverseList(7).take(4).last) ~lutBitsIns(7).take(4).last else lutBitsIns(7).take(4).last
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(3).padTo(3, true).last) dataBitsIn.head.take(3).padTo(3, False).last
      else ~dataBitsIn.head.take(3).padTo(3, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor13240623to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(3, 2, 6, 0, 4, 2, 3, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(2).padTo(6, true),
      getComplementHeap(1).take(2).padTo(6, true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(2).padTo(2, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(5).take(2).padTo(2, true) :+ true,
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(7).take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(2).padTo(6, True),
      dataBitsIn(1).take(2).padTo(6, True),
      dataBitsIn(2).take(6).padTo(6, True),
      dataBitsIn(2).take(4).padTo(4, True) :+ dataBitsIn(2).padTo(6, True).last,
      dataBitsIn(3).take(4).padTo(6, True),
      dataBitsIn(3).take(3).padTo(3, True) ++ dataBitsIn(4).take(2).padTo(2, True),
      dataBitsIn(5).take(3).padTo(3, True) ++ dataBitsIn(4).take(2).padTo(2, True),
      dataBitsIn(5).take(3).padTo(3, True) ++ dataBitsIn(6).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) ^ bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3) ^ bits(4),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) && bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.take(3).count(_ == true) % 2 == 1) ^ (bits(3) && bits(4)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(2).last) ~lutBitsIns.head.take(2).last else lutBitsIns.head.take(2).last,
      if (inverseList(1).take(2).last) ~lutBitsIns(1).take(2).last else lutBitsIns(1).take(2).last,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1,
      if (inverseList(4).take(4).last) ~lutBitsIns(4).take(4).last else lutBitsIns(4).take(4).last,
      lutOuts(5)._1,
      lutOuts(6)._1,
      if (inverseList(7).take(4).last) ~lutBitsIns(7).take(4).last else lutBitsIns(7).take(4).last
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(3).padTo(3, true).last) dataBitsIn.head.take(3).padTo(3, False).last
      else ~dataBitsIn.head.take(3).padTo(3, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06050623to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(3, 2, 6, 0, 5, 0, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(2).padTo(6, true),
      getComplementHeap(1).take(2).padTo(6, true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(5).padTo(6, true),
      getComplementHeap(4).take(4).padTo(5, true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(2).padTo(6, True),
      dataBitsIn(1).take(2).padTo(6, True),
      dataBitsIn(2).take(6).padTo(6, True),
      dataBitsIn(2).take(4).padTo(4, True) :+ dataBitsIn(2).padTo(6, True).last,
      dataBitsIn(3).take(5).padTo(6, True),
      dataBitsIn(3).take(4).padTo(5, True),
      dataBitsIn(4).take(6).padTo(6, True),
      dataBitsIn(4).take(4).padTo(4, True) :+ dataBitsIn(4).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ (bits
                        .take(3)
                        .reduce(_ ^ _) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(2).last) ~lutBitsIns.head.take(2).last else lutBitsIns.head.take(2).last,
      if (inverseList(1).take(2).last) ~lutBitsIns(1).take(2).last else lutBitsIns(1).take(2).last,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1,
      if (inverseList(4).take(5).last) ~lutBitsIns(4).take(5).last else lutBitsIns(4).take(5).last,
      lutOuts(5)._1,
      if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
      lutOuts(7)._1
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(3).padTo(3, true).last) dataBitsIn.head.take(3).padTo(3, False).last
      else ~dataBitsIn.head.take(3).padTo(3, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor14050615to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 1, 6, 0, 5, 0, 4, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(5).padTo(6, true),
      getComplementHeap(4).take(4).padTo(5, true),
      getComplementHeap(6).take(4).padTo(6, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(7).take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(2).take(6).padTo(6, True),
      dataBitsIn(2).take(4).padTo(4, True) :+ dataBitsIn(2).padTo(6, True).last,
      dataBitsIn(3).take(5).padTo(6, True),
      dataBitsIn(3).take(4).padTo(5, True),
      dataBitsIn(4).take(4).padTo(6, True),
      dataBitsIn(4).take(3).padTo(3, True) ++ dataBitsIn(5).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^
                        ((bits
                          .take(3)
                          .count(_ == true) % 2 == 1) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(4).last) ~lutBitsIns.head.take(4).last else lutBitsIns.head.take(4).last,
      if (inverseList(1).take(4).last) ~lutBitsIns(1).take(4).last else lutBitsIns(1).take(4).last,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1,
      if (inverseList(4).take(5).last) ~lutBitsIns(4).take(5).last else lutBitsIns(4).take(5).last,
      lutOuts(5)._1,
      if (inverseList(6).take(4).last) ~lutBitsIns(6).take(4).last else lutBitsIns(6).take(4).last,
      if (inverseList(7).take(4).last) ~lutBitsIns(7).take(4).last else lutBitsIns(7).take(4).last
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor13240615to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 1, 6, 0, 4, 2, 3, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(2).padTo(2, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(5).take(2).padTo(2, true) :+ true,
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(7).take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(2).take(6).padTo(6, True),
      dataBitsIn(2).take(4).padTo(4, True) :+ dataBitsIn(2).padTo(6, True).last,
      dataBitsIn(3).take(4).padTo(6, True),
      dataBitsIn(3).take(3).padTo(3, True) ++ dataBitsIn(4).take(2).padTo(2, True),
      dataBitsIn(5).take(3).padTo(3, True) ++ dataBitsIn(4).take(2).padTo(2, True),
      dataBitsIn(5).take(3).padTo(3, True) ++ dataBitsIn(6).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) ^ bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3) ^ bits(4),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) && bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.take(3).count(_ == true) % 2 == 1) ^ (bits(3) && bits(4)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(4).last) ~lutBitsIns.head.take(4).last else lutBitsIns.head.take(4).last,
      if (inverseList(1).take(4).last) ~lutBitsIns(1).take(4).last else lutBitsIns(1).take(4).last,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1,
      if (inverseList(4).take(4).last) ~lutBitsIns(4).take(4).last else lutBitsIns(4).take(4).last,
      lutOuts(5)._1,
      lutOuts(6)._1,
      if (inverseList(7).take(4).last) ~lutBitsIns(7).take(4).last else lutBitsIns(7).take(4).last
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06050615to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 1, 6, 0, 5, 0, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(5).padTo(6, true),
      getComplementHeap(4).take(4).padTo(5, true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(2).take(6).padTo(6, True),
      dataBitsIn(2).take(4).padTo(4, True) :+ dataBitsIn(2).padTo(6, True).last,
      dataBitsIn(3).take(5).padTo(6, True),
      dataBitsIn(3).take(4).padTo(5, True),
      dataBitsIn(4).take(6).padTo(6, True),
      dataBitsIn(4).take(4).padTo(4, True) :+ dataBitsIn(4).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ (bits
                        .take(3)
                        .reduce(_ ^ _) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(4).last) ~lutBitsIns.head.take(4).last else lutBitsIns.head.take(4).last,
      if (inverseList(1).take(4).last) ~lutBitsIns(1).take(4).last else lutBitsIns(1).take(4).last,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1,
      if (inverseList(4).take(5).last) ~lutBitsIns(4).take(5).last else lutBitsIns(4).take(5).last,
      lutOuts(5)._1,
      if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
      lutOuts(7)._1
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor14141406to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(6, 0, 4, 1, 4, 1, 4, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(5).padTo(6, true),
      getComplementHeap.head.take(4).padTo(5, true),
      getComplementHeap(2).take(4).padTo(6, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(6).take(4).padTo(6, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(7).take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(5).padTo(6, True),
      dataBitsIn.head.take(4).padTo(5, True),
      dataBitsIn(1).take(4).padTo(6, True),
      dataBitsIn(1).take(3).padTo(3, True) ++ dataBitsIn(2).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(3).take(4).padTo(6, True),
      dataBitsIn(3).take(3).padTo(3, True) ++ dataBitsIn(4).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(5).take(4).padTo(6, True),
      dataBitsIn(5).take(3).padTo(3, True) ++ dataBitsIn(6).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^
                        ((bits
                          .take(3)
                          .count(_ == true) % 2 == 1) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        } else {
          if (i % 2 == 0)
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          else
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits(3) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))),
                    6
                  ),
                  invList
                )
            )
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(5).last) ~lutBitsIns.head.take(5).last else lutBitsIns.head.take(5).last,
      lutOuts(1)._1,
      if (inverseList(2).take(4).last) ~lutBitsIns(2).take(4).last else lutBitsIns(2).take(4).last,
      if (inverseList(3).take(4).last) ~lutBitsIns(3).take(4).last else lutBitsIns(3).take(4).last
    ) ++
      (4 until 8).map(i => if (inverseList(i).take(4).last) ~lutBitsIns(i).take(4).last else lutBitsIns(i).take(4).last)

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(6).padTo(6, true).last) dataBitsIn.head.take(6).padTo(6, False).last
      else ~dataBitsIn.head.take(6).padTo(6, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06141406to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(6, 0, 4, 1, 4, 1, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(5).padTo(6, true),
      getComplementHeap.head.take(4).padTo(5, true),
      getComplementHeap(2).take(4).padTo(6, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(5).padTo(6, True),
      dataBitsIn.head.take(4).padTo(5, True),
      dataBitsIn(1).take(4).padTo(6, True),
      dataBitsIn(1).take(3).padTo(3, True) ++ dataBitsIn(2).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(3).take(4).padTo(6, True),
      dataBitsIn(3).take(3).padTo(3, True) ++ dataBitsIn(4).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(5).take(6).padTo(6, True),
      dataBitsIn(5).take(4).padTo(4, True) :+ dataBitsIn(5).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^
                        ((bits
                          .take(3)
                          .count(_ == true) % 2 == 1) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(5).last) ~lutBitsIns.head.take(5).last else lutBitsIns.head.take(5).last,
      lutOuts(1)._1,
      if (inverseList(2).take(4).last) ~lutBitsIns(2).take(4).last else lutBitsIns(2).take(4).last,
      if (inverseList(3).take(4).last) ~lutBitsIns(3).take(4).last else lutBitsIns(3).take(4).last,
      if (inverseList(4).take(4).last) ~lutBitsIns(4).take(4).last else lutBitsIns(4).take(4).last,
      if (inverseList(5).take(4).last) ~lutBitsIns(5).take(4).last else lutBitsIns(5).take(4).last,
      if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(6).padTo(6, true).last) dataBitsIn.head.take(6).padTo(6, False).last
      else ~dataBitsIn.head.take(6).padTo(6, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06221406to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(6, 0, 4, 1, 2, 2, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(5).padTo(6, true),
      getComplementHeap.head.take(4).padTo(5, true),
      getComplementHeap(2).take(4).padTo(6, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(2).padTo(6, true),
      getComplementHeap(5).take(2).padTo(6, true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(5).padTo(6, True),
      dataBitsIn.head.take(4).padTo(5, True),
      dataBitsIn(1).take(4).padTo(6, True),
      dataBitsIn(1).take(3).padTo(3, True) ++ dataBitsIn(2).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(3).take(2).padTo(6, True),
      dataBitsIn(4).take(2).padTo(6, True),
      dataBitsIn(5).take(6).padTo(6, True),
      dataBitsIn(5).take(4).padTo(4, True) :+ dataBitsIn(5).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^
                        ((bits
                          .take(3)
                          .count(_ == true) % 2 == 1) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(5).last) ~lutBitsIns.head.take(5).last else lutBitsIns.head.take(5).last,
      lutOuts(1)._1,
      if (inverseList(2).take(4).last) ~lutBitsIns(2).take(4).last else lutBitsIns(2).take(4).last,
      if (inverseList(3).take(4).last) ~lutBitsIns(3).take(4).last else lutBitsIns(3).take(4).last,
      if (inverseList(4).take(2).last) ~lutBitsIns(4).take(2).last else lutBitsIns(4).take(2).last,
      if (inverseList(5).take(2).last) ~lutBitsIns(5).take(2).last else lutBitsIns(5).take(2).last,
      if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(6).padTo(6, true).last) dataBitsIn.head.take(6).padTo(6, False).last
      else ~dataBitsIn.head.take(6).padTo(6, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor14141325to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 2, 3, 1, 4, 1, 4, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(2).padTo(2, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(1).take(2).padTo(2, true) :+ true,
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(6).take(4).padTo(6, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(7).take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(2).padTo(2, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(1).take(2).padTo(2, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(4).take(4).padTo(6, True),
      dataBitsIn(4).take(3).padTo(3, True) ++ dataBitsIn(5).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(6).take(4).padTo(6, True),
      dataBitsIn(6).take(3).padTo(3, True) ++ dataBitsIn(7).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) ^ bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3) ^ bits(4),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) && bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.take(3).count(_ == true) % 2 == 1) ^ (bits(3) && bits(4)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        } else {
          if (i % 2 == 0)
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          else
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits(3) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))),
                    6
                  ),
                  invList
                )
            )
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList(0).take(4).last) ~lutBitsIns(0).take(4).last else lutBitsIns(0).take(4).last,
      lutOuts(1)._1,
      lutOuts(2)._1,
      if (inverseList(3).take(4).last) ~lutBitsIns(3).take(4).last else lutBitsIns(3).take(4).last
    ) ++
      (4 until 8).map(i => if (inverseList(i).take(4).last) ~lutBitsIns(i).take(4).last else lutBitsIns(i).take(4).last)

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06141325to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 2, 3, 1, 4, 1, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(2).padTo(2, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(1).take(2).padTo(2, true) :+ true,
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(2).padTo(2, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(1).take(2).padTo(2, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(4).take(4).padTo(6, True),
      dataBitsIn(4).take(3).padTo(3, True) ++ dataBitsIn(5).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(6).take(6).padTo(6, True),
      dataBitsIn(6).take(4).padTo(4, True) :+ dataBitsIn(6).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) ^ bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3) ^ bits(4),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) && bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.take(3).count(_ == true) % 2 == 1) ^ (bits(3) && bits(4)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList(0).take(4).last) ~lutBitsIns(0).take(4).last else lutBitsIns(0).take(4).last,
      lutOuts(1)._1,
      lutOuts(2)._1,
      if (inverseList(3).take(4).last) ~lutBitsIns(3).take(4).last else lutBitsIns(3).take(4).last,
      if (inverseList(4).take(4).last) ~lutBitsIns(4).take(4).last else lutBitsIns(4).take(4).last,
      if (inverseList(5).take(4).last) ~lutBitsIns(5).take(4).last else lutBitsIns(5).take(4).last,
      if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06221325to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(5, 2, 3, 1, 2, 2, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(4).padTo(6, true),
      getComplementHeap.head.take(3).padTo(3, true) ++ getComplementHeap(1).take(2).padTo(2, true),
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(1).take(2).padTo(2, true) :+ true,
      getComplementHeap(2).take(3).padTo(3, true) ++ getComplementHeap(3).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(4).take(2).padTo(6, true),
      getComplementHeap(5).take(2).padTo(6, true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(4).padTo(6, True),
      dataBitsIn.head.take(3).padTo(3, True) ++ dataBitsIn(1).take(2).padTo(2, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(1).take(2).padTo(2, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(4).take(2).padTo(6, True),
      dataBitsIn(5).take(2).padTo(6, True),
      dataBitsIn(6).take(6).padTo(6, True),
      dataBitsIn(6).take(4).padTo(4, True) :+ dataBitsIn(6).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) ^ bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3) ^ bits(4),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => bits(3) && bits(4),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.take(3).count(_ == true) % 2 == 1) ^ (bits(3) && bits(4)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case _ =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => (bits.head && bits(1) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList(0).take(4).last) ~lutBitsIns(0).take(4).last else lutBitsIns(0).take(4).last,
      lutOuts(1)._1,
      lutOuts(2)._1,
      if (inverseList(3).take(4).last) ~lutBitsIns(3).take(4).last else lutBitsIns(3).take(4).last,
      if (inverseList(4).take(2).last) ~lutBitsIns(4).take(2).last else lutBitsIns(4).take(2).last,
      if (inverseList(5).take(2).last) ~lutBitsIns(5).take(2).last else lutBitsIns(5).take(2).last,
      if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(5).padTo(5, true).last) dataBitsIn.head.take(5).padTo(5, False).last
      else ~dataBitsIn.head.take(5).padTo(5, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor14140606to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(6, 0, 6, 0, 4, 1, 4, 1)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(5).padTo(6, true),
      getComplementHeap.head.take(4).padTo(5, true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(6).take(4).padTo(6, true),
      getComplementHeap(6).take(3).padTo(3, true) ++ getComplementHeap(7).take(1).padTo(1, true) ++ Seq.fill(2)(true)
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(5).padTo(6, True),
      dataBitsIn.head.take(4).padTo(5, True),
      dataBitsIn(1).take(6).padTo(6, True),
      dataBitsIn(1).take(4).padTo(4, True) :+ dataBitsIn(1).padTo(6, True).last,
      dataBitsIn(2).take(4).padTo(6, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(4).take(4).padTo(6, True),
      dataBitsIn(4).take(3).padTo(3, True) ++ dataBitsIn(5).take(1).padTo(1, True) ++ Seq.fill(2)(True)
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ (bits
                        .take(3)
                        .reduce(_ ^ _) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          if (i % 2 == 0)
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits.take(4).count(_ == true) % 2 == 1,
                    6
                  ),
                  invList
                )
            )
          else
            LUT6_2.process(
              bitIns,
              LUTUtils
                .getValueWithInverse(
                  LUTUtils.expression2value(
                    bits => bits(3) ^ ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))),
                    6
                  ),
                  invList
                )
            )
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(5).last) ~lutBitsIns.head.take(5).last else lutBitsIns.head.take(5).last,
      lutOuts(1)._1,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1
    ) ++
      (4 until 8).map(i => if (inverseList(i).take(4).last) ~lutBitsIns(i).take(4).last else lutBitsIns(i).take(4).last)

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(6).padTo(6, true).last) dataBitsIn.head.take(6).padTo(6, False).last
      else ~dataBitsIn.head.take(6).padTo(6, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06220606to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(6, 0, 6, 0, 2, 2, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(5).padTo(6, true),
      getComplementHeap.head.take(4).padTo(5, true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(2).padTo(6, true),
      getComplementHeap(5).take(2).padTo(6, true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(5).padTo(6, True),
      dataBitsIn.head.take(4).padTo(5, True),
      dataBitsIn(1).take(6).padTo(6, True),
      dataBitsIn(1).take(4).padTo(4, True) :+ dataBitsIn(1).padTo(6, True).last,
      dataBitsIn(2).take(2).padTo(6, True),
      dataBitsIn(3).take(2).padTo(6, True),
      dataBitsIn(4).take(6).padTo(6, True),
      dataBitsIn(4).take(4).padTo(4, True) :+ dataBitsIn(4).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ (bits
                        .take(3)
                        .reduce(_ ^ _) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(2).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(5).last) ~lutBitsIns.head.take(5).last else lutBitsIns.head.take(5).last,
      lutOuts(1)._1,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1,
      if (inverseList(4).take(2).last) ~lutBitsIns(4).take(2).last else lutBitsIns(4).take(2).last,
      if (inverseList(5).take(2).last) ~lutBitsIns(5).take(2).last else lutBitsIns(5).take(2).last,
      if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(6).padTo(6, true).last) dataBitsIn.head.take(6).padTo(6, False).last
      else ~dataBitsIn.head.take(6).padTo(6, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

case class Compressor06140606to9(override val complementHeap: Seq[Seq[Boolean]] = null) extends Gpc {

  override def inputFormat = Seq(6, 0, 6, 0, 4, 1, 6, 0)

  override def outputFormat = Seq.fill(9)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 8, ff = outputFormat.sum, carry8 = 1)

  override def implH = new ChainsawOperatorModule(this) {
    val inverseList = Seq(
      getComplementHeap.head.take(5).padTo(6, true),
      getComplementHeap.head.take(4).padTo(5, true),
      getComplementHeap(2).take(6).padTo(6, true),
      getComplementHeap(2).take(4).padTo(4, true) :+ getComplementHeap(2).padTo(6, true).last,
      getComplementHeap(4).take(4).padTo(6, true),
      getComplementHeap(4).take(3).padTo(3, true) ++ getComplementHeap(5).take(1).padTo(1, true) ++ Seq.fill(2)(true),
      getComplementHeap(6).take(6).padTo(6, true),
      getComplementHeap(6).take(4).padTo(4, true) :+ getComplementHeap(6).padTo(6, true).last
    ).map(_.map(!_))

    val dataBitsIn = dataIn.map(_.raw.asBools) // low to high

    val lutBitsIns = Seq(
      dataBitsIn.head.take(5).padTo(6, True),
      dataBitsIn.head.take(4).padTo(5, True),
      dataBitsIn(1).take(6).padTo(6, True),
      dataBitsIn(1).take(4).padTo(4, True) :+ dataBitsIn(1).padTo(6, True).last,
      dataBitsIn(2).take(4).padTo(6, True),
      dataBitsIn(2).take(3).padTo(3, True) ++ dataBitsIn(3).take(1).padTo(1, True) ++ Seq.fill(2)(True),
      dataBitsIn(4).take(6).padTo(6, True),
      dataBitsIn(4).take(4).padTo(4, True) :+ dataBitsIn(4).padTo(6, True).last
    )

    val lutOuts = inverseList
      .zip(lutBitsIns)
      .zipWithIndex
      .map { case ((invList, bitIns), i) =>
        if (i <= 3) {
          i match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(5).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ (bits
                        .take(3)
                        .reduce(_ ^ _) && bits(3)),
                    invList
                  )
                )
              ).process(bitIns: _*)
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        } else {
          i - 4 match {
            case 0 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(4).reduce(_ ^ _),
                      6
                    ),
                    invList
                  )
              )
            case 1 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => ((bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1)))) ^ bits(3),
                      6
                    ),
                    invList
                  )
              )
            case 2 =>
              LUT6_2.process(
                bitIns,
                LUTUtils
                  .getValueWithInverse(
                    LUTUtils.expression2value(
                      bits => bits.take(6).count(_ == true) % 2 == 1,
                      6
                    ),
                    invList
                  )
              )
            case _ =>
              LUT5to2(
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits => (bits.head && bits(1)) || (bits(2) && (bits.head ^ bits(1))),
                    invList
                  )
                ),
                toLUT5_2Format(
                  LUTUtils.getExpressionWithInverse(
                    bits =>
                      ((bits.take(3).reduce(_ ^ _) && bits(3)) ^ ((bits.head && bits(1)) || (bits(
                        2
                      ) && (bits.head ^ bits(
                        1
                      ))))) ^ (bits(4) && bits.take(4).reduce(_ ^ _)),
                    invList
                  )
                )
              ).process(bitIns: _*)
          }
        }
      }
      .map(seq => (seq(0), seq(1)))

    val carryChain = CARRY8()
    val selects    = lutOuts.map(_._2)
    val data = Seq(
      if (inverseList.head.take(5).last) ~lutBitsIns.head.take(5).last else lutBitsIns.head.take(5).last,
      lutOuts(1)._1,
      if (inverseList(2).take(5).last) ~lutBitsIns(2).take(5).last else lutBitsIns(2).take(5).last,
      lutOuts(3)._1,
      if (inverseList(4).take(4).last) ~lutBitsIns(4).take(4).last else lutBitsIns(4).take(4).last,
      if (inverseList(5).take(4).last) ~lutBitsIns(5).take(4).last else lutBitsIns(5).take(4).last,
      if (inverseList(6).take(5).last) ~lutBitsIns(6).take(5).last else lutBitsIns(6).take(5).last,
      lutOuts.last._1
    )

    (0 until 8).foreach { j =>
      carryChain.DI(j) := data(j)
      carryChain.S(j)  := selects(j)
    }
    val cIn =
      if (getComplementHeap.head.take(6).padTo(6, true).last) dataBitsIn.head.take(6).padTo(6, False).last
      else ~dataBitsIn.head.take(6).padTo(6, False).last
    carryChain.CI     := cIn
    carryChain.CI_TOP := False
    dataOut.head      := (carryChain.CO(7) ## carryChain.O).asUInt.toAFix
  }
}

object Gpcs {
  def apply(): Seq[Gpc] = Seq(
    Compressor6to3(),
    Compressor3to2(),
    Compressor5to3(),
    Compressor14to3(),
    Compressor15to3(),
    Compressor23to3(),
    Compressor06050615to9(),
    Compressor06050623to9(),
    Compressor06051415to9(),
    Compressor06140606to9(),
    Compressor06141325to9(),
    Compressor06141406to9(),
    Compressor06220606to9(),
    Compressor06221325to9(),
    Compressor06221406to9(),
    Compressor13240623to9(),
    Compressor13241415to9(),
    Compressor13240615to9(),
    Compressor14050623to9(),
    Compressor14051415to9(),
    Compressor14050615to9(),
    Compressor14140606to9(),
    Compressor14141325to9(),
    Compressor14141406to9()
  )
}
