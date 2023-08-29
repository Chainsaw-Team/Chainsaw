package Chainsaw.phases

import Chainsaw._
import Chainsaw.edaFlow._
import spinal.core._
import spinal.core.internals._

import scala.collection.mutable

/** contains and format util statistics of a component(not including its children)
  * @param name
  *   name of the component, for every unique generator, the name should be different
  * @param multStatistics
  *   statistics of multipliers
  * @param addSubStatistics
  *   statistics of adders and subtractors
  * @param memStatistics
  *   memory statistics, including the number of read ports, write ports, width and depth
  */
case class ComponentUtilStats(
    name: String,
    multStatistics: Stat                                     = mutable.Map[(Int, Int), Int](),
    addSubStatistics: Stat                                   = mutable.Map[(Int, Int), Int](),
    memStatistics: mutable.Map[Mem[_], (Int, Int, Int, Int)] = mutable.Map[Mem[_], (Int, Int, Int, Int)]()
) {

  def merge(a: Stat, b: Stat) = {
    val ret = a.clone()
    b.foreach { case (k, v) => ret.update(k, a.getOrElse(k, 0) + v) }
    ret
  }
  def +(that: ComponentUtilStats): ComponentUtilStats = {
    ComponentUtilStats(
      name,
      merge(multStatistics, that.multStatistics),
      merge(addSubStatistics, that.addSubStatistics),
      memStatistics ++ that.memStatistics
    )
  }

  def withName(newName: String) = ComponentUtilStats(newName, multStatistics, addSubStatistics, memStatistics)

  def fit(size: (Int, Int), target: (Int, Int)): Boolean = size._1 <= target._1 && size._2 <= target._2

  // FPGA stats

  val dspUpperBound: (Int, Int) = targetDeviceFamily match {
    case family: XilinxDeviceFamily =>
      family match {
        case UltraScale => (18, 27)
        case Series7    => (18, 25)
      }
    case altera: AlteraDeviceFamily => (27, 27)
    case Generic                    => (32, 32)
  }

  val dspLowserBound = (4, 4)

  def formatMultForFpga: Seq[Int] = {
    val (good, oversize) = multStatistics.partition { case (size, _) => fit(size, dspUpperBound) }
    val (comb, dsp)      = good.partition { case (size, _) => fit(size, dspLowserBound) }
    Seq(comb.values.sum, dsp.values.sum, oversize.values.sum)
  }

  // TODO: classification of addSub - good/oversize

  /** util of addSub is the sum of operation widths
    */
  def formatAddForFpga: Seq[Int] = {
    Seq(addSubStatistics.map { case ((l, r), i) => r * i }.sum) // r >= l
  }

  // TODO: classification of mem - LUT/BRAM/URAM
  // FIXME: proper bound for URAM and BRAM

  val uramBound = (512, 512)
  val bramBound = (512, 512)

  def formatMemForFpga: Seq[Int] = {

    val bits = memStatistics.map { case (_, (_, _, width, depth)) => width * depth }.sum
    Seq(bits)

//    val (rom, ram) = memStatistics.partition { case (_, (_, wC, _, _)) => wC == 0 }
//    // rom classification
//    val (singleRom, multiRom) = rom.partition { case (_, (rC, wC, _, _)) => rC <= 1 && wC <= 1 }
//    val (tempRom, urom)       = singleRom.partition { case (_, (_, _, width, depth)) => fit((width, depth), uramBound) }
//    val (lutRom, brom)        = tempRom.partition { case (_, (_, _, width, depth)) => fit((width, depth), bramBound) }
//
//    // ram classification
//    val (single, multi) = ram.partition { case (_, (rC, wC, _, _)) => rC <= 1 && wC <= 1 }
//    val (temp, uram)    = single.partition { case (_, (_, _, width, depth)) => fit((width, depth), uramBound) }
//    val (lut, bram)     = temp.partition { case (_, (_, _, width, depth)) => fit((width, depth), bramBound) }
//
//    Seq(lutRom.size, brom.size, urom.size, multiRom.size, lut.size, bram.size, uram.size, multi.size)
  }

  def getStatsForFpga: Seq[Int] = formatMultForFpga ++ formatAddForFpga ++ formatMemForFpga

  // TODO: ASIC stats

}

object ComponentUtilStats {

  def printTable(info: Seq[(Int, ComponentUtilStats)]) = {

    logger.info("Component Utilization By Hierarchy")
    val total = (0, info.map(_._2).reverse.reduce(_ + _).withName("total"))

    // by hierarchy
    val componentMax = info.map { case (i, stats) => i * 2 + stats.name.length + 2 }.max
    val multItems    = Seq("Tiny Mult", "Dsp Mult", "Big Mult")
    val addSubItems  = Seq("AddSub")
//    val memItems           = Seq("LutRom", "Brom", "Urom", "MultiPort ROM", "LutRam", "Bram", "Uram", "MultiPort RAM")
    val memItems           = Seq("RAM/ROM (bit)")
    val items: Seq[String] = Seq("Component") ++ multItems ++ addSubItems ++ memItems

    val digitWidths: Seq[Seq[Int]] = info.map { case (_, stats) => stats.getStatsForFpga.map(_.toString.length) } :+
      items.tail.map(_.length + 2)
    val digitMax: Seq[Int] = digitWidths.transpose.map(_.max)
    val columnWidths       = componentMax +: digitMax

    val header = items.zip(columnWidths).map { case (item, width) => item.padTo(width, ' ') }.mkString

    val lines = (total +: info)
      .map { case (depth, stats) =>
        val namsPart = (" " * 2 * depth) + stats.name
        val allParts = namsPart +: stats.getStatsForFpga.map(_.toString)
        allParts.zip(columnWidths).map { case (item, width) => item.padTo(width, ' ') }.mkString
      }
      .mkString("\n")

    println(header)
    println(lines)

    // TODO: table by generator

    // by category
    logger.info("Component Utilization By Category")
    val groups           = info.groupBy(_._2.name)
    val itemsByCategory  = (items.head +: Seq("Count")) ++ items.tail
    val widthsByCategory = (columnWidths.head +: Seq(7)) ++ columnWidths.tail
    val headerByCategory =
      itemsByCategory.zip(widthsByCategory).map { case (item, width) => item.padTo(width, ' ') }.mkString

    val linesByCategory = groups
      .map { case (name, seq) =>
        val util     = seq.map(_._2).reduce(_ + _)
        val count    = seq.size
        val allParts = (name +: Seq(count.toString)) ++ util.getStatsForFpga.map(_.toString)
        allParts.zip(widthsByCategory).map { case (item, width) => item.padTo(width, ' ') }.mkString
      }
      .mkString("\n")

    println(headerByCategory)
    println(linesByCategory)

  }
}

/** estimate the utilization and fmax for generic ASIC/FPGA
  */
class GenericEstimation extends Phase {

  // TODO: verify that all statements and expressions can be accessed by this method once and only once
  override def impl(pc: PhaseContext): Unit = {

    logger.info("start applying timing DRC")

    val trace               = mutable.Queue[String]()
    val componentStatistics = mutable.ArrayBuffer[(Int, ComponentUtilStats)]()

    val top                         = pc.topLevel
    var currentComponent: Component = top
    var currentLevel                = -1

    recComponent(top)
    ComponentUtilStats.printTable(componentStatistics.reverse)

    def stat = componentStatistics.last._2

    def recComponent(c: Component): Unit = {
      trace += c.toString()
      currentLevel += 1
      c.children.foreach(recComponent)

      currentComponent = c
      componentStatistics += ((currentLevel, ComponentUtilStats(c.name)))
      c.dslBody.foreachStatements(recStatement)

      trace.dequeue()
      currentLevel -= 1
    }

    def recStatement(s: Statement): Unit = {
      trace += s.toString
      s.foreachExpression(recExpression)
      s match {
        case ts: TreeStatement => ts.foreachStatements(recStatement)
        case memPort: MemWrite => updateMemStats(stat.memStatistics, memPort.mem, read = false)
        case _                 =>
      }
      trace.dequeue()
    }

    def getBinarySize(op: BinaryOperatorWidthableInputs): (Int, Int) = {
      val size = Seq(op.left, op.right).map(_.getWidth).sorted
      (size.head, size.last)
    }

    def getMemSize(mem: Mem[_]): (Int, Int) = (mem.width, 1 << mem.addressWidth)

    def updateBinaryStats(stat: mutable.Map[(Int, Int), Int], key: (Int, Int)): Unit = {
      stat.update(key, stat.getOrElse(key, 0) + 1)
    }

    def updateMemStats(stat: mutable.Map[Mem[_], (Int, Int, Int, Int)], mem: Mem[_], read: Boolean) = {
      val (width, depth)                = getMemSize(mem)
      val (readCount, writeCount, _, _) = stat.getOrElse(mem, (0, 0, 0, 0))
      if (read) stat.update(mem, (readCount + 1, writeCount, width, depth))
      else stat.update(mem, (readCount, writeCount + 1, width, depth))
    }

    // FIXME: can't traverse MemWrite by current rec methods
    def recExpression(e: Expression): Unit = {
      trace += e.toString()
      // TODO: find MUXes
      e match {
        case op: Operator.BitVector.Mul       => updateBinaryStats(stat.multStatistics, getBinarySize(op))
        case op: Operator.BitVector.Add       => updateBinaryStats(stat.addSubStatistics, getBinarySize(op))
        case op: Operator.BitVector.Sub       => updateBinaryStats(stat.addSubStatistics, getBinarySize(op))
        case op: Operator.SInt.SmallerOrEqual => updateBinaryStats(stat.addSubStatistics, getBinarySize(op))
        case op: Operator.SInt.Smaller        => updateBinaryStats(stat.addSubStatistics, getBinarySize(op))
        case memPort: MemReadSync             => updateMemStats(stat.memStatistics, memPort.mem, read = true)
        case memPort: MemReadAsync            => updateMemStats(stat.memStatistics, memPort.mem, read = true)
        case _                                =>
      }
      e.foreachExpression(recExpression)
      trace.dequeue()
    }
  }

  override def hasNetlistImpact = false

  override def toString = s"${super.toString}"
}
