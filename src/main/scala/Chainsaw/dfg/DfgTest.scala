package Chainsaw.dfg

import spinal.core.sim.{SimConfig, _}
import spinal.core.{Component, Vec, in, out}
import Chainsaw.arithmetic.floating._
import Chainsaw.examples.{StreamPeek, StreamPeekFloating, StreamPoke, StreamPokeFloating}

import scala.collection.mutable.ArrayBuffer
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

object DfgRawTest {
  def apply(dfg: => Dfg, stimulus: Seq[Seq[Float]]) = {
    val ret = ArrayBuffer[Seq[Float]]()
    SimConfig.withFstWave.compile(DfgRawWrapper(dfg)).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}

      dataIn.foreach { port => port #= 0.0 }
      clockDomain.forkStimulus(2)

      stimulus.map { inputs =>
        inputs.zip(dataIn).foreach { case (input, port) => port #= input }
        clockDomain.waitSampling()
        ret += dataOut.map(_.toFloat)
      }

    }
    ret

  }
}

object DfgStreamTest {
  def apply(dfg: => Dfg, stimulus: Seq[Seq[Float]], golden:Seq[Seq[Float]]) = {
    SimConfig.withFstWave.compile(DfgStreamWrapper(dfg)).doSim { dut =>

      val scoreboards = dut.dataOut.map(_ => ScoreboardInOrder[Float]())

      // input
      dut.dataIn.zipWithIndex.foreach{ case (stream, i) =>
        StreamPokeFloating(stream, dut.clockDomain, stimulus.map(_.apply(i)))
      }

      // output
      scoreboards.zipWithIndex.foreach{ case (board, i) => golden.map(_.apply(i)).foreach(board.pushRef)}
      dut.dataOut.zip(scoreboards) foreach { case (stream, board) =>
        StreamPeekFloating(stream, dut.clockDomain, board)
      }

      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSamplingWhere(scoreboards.head.matches == golden.length)
      println(scoreboards.head.matches)

    }
  }
}

object DfgRawWrapper {

  def apply(dfg: => Dfg) = {
    new Component {
      val graph   = dfg
      val dataIn  = in(Vec(graph.floatingInputs.values.map(_.payload))) // TODO: order?
      val dataOut = out(Vec(graph.floatingOutputs.values.map(_.payload)))
      graph.build()
    }
  }
}

object DfgStreamWrapper {

  def apply(dfg: => Dfg) = {
    new Component {
      val graph = dfg
      val dataIn = graph.floatingInputs.values.map { stream =>
        val input = slave(cloneOf(stream))
        input >> stream
        input.simPublic()
        input
      }.toSeq

      val dataOut = graph.floatingOutputs.values.map {stream =>
        val output = master(cloneOf(stream))
        stream >> output
        output.simPublic()
        output
      }.toSeq
      graph.useStream = true
      graph.build()
    }
  }
}