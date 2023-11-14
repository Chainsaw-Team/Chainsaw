package Chainsaw.edaFlow.vcs
import Chainsaw._
import Chainsaw.edaFlow._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import java.io.File
import scala.collection.mutable

class VcsFlowTest extends AnyFunSuite {
  test("test vcsFlow for StreamFifo") {
    VcsFlow(
      new ChainsawEdaDirInput(Seq[File](), new File(simWorkspace, "vcs"), "StreamFifo"),
      compileOption = VcsCompileOption(
        Seq(FullCoverage),
        parallelNumber     = 8,
        incrementCompile   = true,
        enableMemHierarchy = true,
        noTimingCheck      = true
      )
    ).getSpinalSimConfig()
      .compile(
        rtl = new StreamFifo(
          dataType = Bits(32 bits),
          depth    = 32
        )
      )
      .doSimUntilVoid { dut =>
        val queueModel = mutable.Queue[Long]()

        dut.clockDomain.forkStimulus(period = 10)
        SimTimeout(1000000 * 10)
        // Push data randomly, and fill the queueModel with pushed transactions.
        val pushThread = fork {

          dut.io.push.valid #= false
          while (true) {
            dut.io.push.valid.randomize()
            dut.io.push.payload.randomize()
            dut.clockDomain.waitSampling()
            if (dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean) {
              queueModel.enqueue(dut.io.push.payload.toLong)
            }
          }
        }

        // Pop data randomly, and check that it match with the queueModel.
        val popThread = fork {
          dut.io.pop.ready #= true
          for (i <- 0 until 100000) {
            dut.io.pop.ready.randomize()
            dut.clockDomain.waitSampling()
            if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) {
              assert(dut.io.pop.payload.toLong == queueModel.dequeue())
            }
          }
          simSuccess()
        }
      }
  }
}
