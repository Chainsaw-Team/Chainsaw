package Chainsaw.examples

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

import scala.collection.mutable
import scala.util.Random
import Chainsaw.NumericExt._
import Chainsaw._

import scala.collection.mutable.ArrayBuffer

class StreamExampleTest extends AnyFlatSpec {

  def getStimulus: Seq[Double]        = Seq.fill(100)(Random.nextInt(16).toDouble)
  def getBooleanStimulus: Seq[Double] = Seq.fill(100)(Random.nextInt(2).toDouble)

  "Stream" should "work in a delay line with back pressure" in {

    val stimulus = getStimulus

    (0 until 10).foreach { _ =>
      SimConfig.withFstWave.compile(DelayLineWithBackPressure()).doSim { dut =>
        val scoreboard = ScoreboardInOrder[Double]()
        stimulus.foreach(scoreboard.pushRef)

        // input
        StreamPoke(dut.dataIn, dut.clockDomain, stimulus)

        // output
        StreamPeek(dut.dataOut, dut.clockDomain, scoreboard)

        dut.clockDomain.forkStimulus(2)
        dut.clockDomain.waitSamplingWhere(scoreboard.matches == stimulus.length)
      }
    }

  }

  it should "work with StreamForkAndJoin" in {

    val a    = getStimulus
    val b    = getStimulus
    val c    = getStimulus
    val sum0 = a.zip(c).map { case (d, d1) => d + d1 }
    val sum1 = b.zip(c).map { case (d, d1) => d + d1 }

    (0 until 10).foreach { _ =>
      SimConfig.withFstWave.compile(StreamForkAndJoinExample()).doSim { dut =>
        val scoreboard0 = ScoreboardInOrder[Double]()
        val scoreboard1 = ScoreboardInOrder[Double]()
        sum0.foreach(scoreboard0.pushRef)
        sum1.foreach(scoreboard1.pushRef)

        // input
        StreamPoke(dut.a, dut.clockDomain, a)
        StreamPoke(dut.b, dut.clockDomain, b)
        StreamPoke(dut.c, dut.clockDomain, c)

        // output
        StreamPeek(dut.d, dut.clockDomain, scoreboard0)
        StreamPeek(dut.e, dut.clockDomain, scoreboard1)

        dut.clockDomain.forkStimulus(2)
        dut.clockDomain.waitSamplingWhere(scoreboard0.matches == sum0.length && scoreboard1.matches == sum1.length)
      }
    }

  }

  it should "work with StreamMux" in {

    val a   = getStimulus
    val b   = getStimulus
    val sel = getBooleanStimulus
//    val ret = a.zip(b).zip(sel).map{ case ((a,b), sel) => if (sel == 0.0) a  else b} // wrong behavior
    val ret  = ArrayBuffer[Double]()
    var i, j = 0
    sel.foreach { s =>
      if (s == 0.0) {
        ret += a(i)
        i += 1
      } else {
        ret += b(j)
        j += 1
      }
    }

    (0 until 10).foreach { _ =>
      SimConfig.withFstWave.compile(StreamMuxSafeExample()).doSim { dut =>
        val scoreboard = ScoreboardInOrder[Double]()
        ret.foreach(scoreboard.pushRef)

        // input
        StreamPoke(dut.a, dut.clockDomain, a)
        StreamPoke(dut.b, dut.clockDomain, b)
        StreamPoke(dut.sel, dut.clockDomain, sel)

        // output
        StreamPeek(dut.c, dut.clockDomain, scoreboard)

        dut.clockDomain.forkStimulus(2)
        dut.clockDomain.waitSamplingWhere(scoreboard.matches == ret.length)
      }
    }

  }

  // TODO: test P2SCC with StreamUtils
  it should "work with P2SCC" in {

    SimConfig.withFstWave.compile(new StreamP2SCC(8, 3)).doSim { dut =>
      dut.domainSlow.forkStimulus(6)
      dut.domainFast.forkStimulus(2)

      dut.slow.valid #= false
      dut.fast.ready #= false
      dut.domainSlow.waitSampling()

      dut.fast.ready #= true
      (0 until 10).foreach { i =>
        if (i < 10) {
          (0 until 3).foreach { j =>
            dut.slow.valid      #= true
            dut.slow.payload(j) #= i * 3 + j
          }
          dut.slow.valid #= true
        } else {
          dut.slow.valid #= false
        }
        dut.domainSlow.waitSampling()
      }
      dut.domainSlow.waitSampling(10)

    }

  }
}
