package Chainsaw.examples

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

case class PipelineExample() extends Component {

  val io = new Bundle {
    val data_in  = slave Flow (UInt(8 bits))
    val side_in  = slave Flow (UInt(8 bits))
    val data_out = master Flow (UInt(8 bits))
  }

  noIoPrefix()

  val pip = new Pipeline {

    val payload = Stageable(UInt(8 bits))
    val side    = Stageable(UInt(8 bits))
    val sum     = Stageable(UInt(8 bits))

    val stage0 = new Stage {
      this.internals.input.valid := io.data_in.valid
      this(payload)              := io.data_in.payload
    }
    val stage1 = new Stage(Connection.M2S()) {
      this.internals.input.valid.allowOverride := stage0.valid && io.side_in.valid
      this(side)                               := io.side_in.payload
    }
    val stage2 = new Stage(Connection.M2S()) {
      this(sum) := this(payload) + this(side)
    }
    val stage3 = new Stage(Connection.M2S()) {
      io.data_out.valid   := this.internals.output.valid
      io.data_out.payload := this(sum)
    }
  }
  pip.build()
}

object PipelineExample extends App {
  SpinalConfig().generateVerilog(PipelineExample())
}
