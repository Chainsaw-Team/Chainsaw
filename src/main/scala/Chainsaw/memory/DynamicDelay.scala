package Chainsaw.memory

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class DynamicDelay(width: Int, delayMax: Int, dynamic: Boolean = false) extends Component {

  val counterWidth = log2Up(delayMax + 1)
  val paddedLength = Pow2(counterWidth)

  val dataIn = in Bits (width bits)
  val dataOut = out Bits (width bits)
  val delayIn = in UInt (counterWidth bits)
  val lockedOut = out Bool()

  val index = CounterFreeRun(paddedLength)
  val ram = Mem(Bits(width bits), paddedLength)
  ram.setAsBlockRam()
  ram.write(index.value, dataIn)
  val readAddr = (index.value - (delayIn - 3)).d()
  val readData = ram.readSync(readAddr).d()

  val changed = delayIn =/= delayIn.d()

  val fsm = new StateMachine {
    val unlockCounter = Counter(paddedLength)
    val UNLOCKED = makeInstantEntry()
    val LOCKED = State()
    LOCKED.whenIsActive(when(changed)(goto(UNLOCKED)))
    UNLOCKED.onEntry(unlockCounter.clear())
    UNLOCKED.whenIsActive {
      unlockCounter.increment()
      when(unlockCounter.value === delayIn)(goto(LOCKED))
    }

    lockedOut := isActive(LOCKED)
    dataOut := Mux(isActive(LOCKED), readData, B(0))
  }
}
