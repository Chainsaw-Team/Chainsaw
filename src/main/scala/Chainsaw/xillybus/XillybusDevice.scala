package Chainsaw.xillybus

/** a xillybus device file
  *
  * @param name
  *   the name you specify through xillybus IP factory [[http://xillybus.com/custom-ip-factory]]
  * @param deviceType
  * = "fifo"/"mem" for stream(FIFO) or address based(RegFile or Mem) port
  * @param direction
  * = "write"/"read" for write (Host -> FPGA) or read (FPGA -> Host)
  * @param bitWidth
  * = 8/16/32, bit width of the fifo/register, can be
  * @param addrWidth
  *   for address based port, width of address
  */
case class XillybusDevice(
    name: String,
    deviceType: String,
    direction: String,
    bitWidth: Int,
    addrWidth: Int = -1,
    depth: Int     = -1
) {

  deviceType match {
    case "fifo" =>
    case "mem"  => require(addrWidth != -1)
    case _      => throw new IllegalArgumentException("device type must be fifo/mem")
  }

  val directionName = direction match {
    case "write" => "w"
    case "read"  => "r"
    case "bi"    => ""
    case _       => throw new IllegalArgumentException("device type must be w/r/bi")
  }

  def fullName = s"user_${directionName}_$name"

  def deviceName = s"\\\\.\\xillybus_$name" // device name in windows system

  def getFifoDepth: Int = deviceType match {
    case "fifo" => if (depth == -1) 2048 / (bitWidth / 8) else depth
    case "mem"  => throw new IllegalAccessError("mem device has no FIFO depth")
  }
}

// factory methods

object XillybusFifoRead {
  def apply(name: String, bitWidth: Int, depth: Int = -1) = XillybusDevice(name, "fifo", "read", bitWidth, depth=depth)
}

object XillybusFifoWrite {
  def apply(name: String, bitWidth: Int, depth: Int = -1) = XillybusDevice(name, "fifo", "write", bitWidth, depth=depth)
}

object XillybusMemWrite {
  def apply(name: String, bitWidth: Int, addrWidth: Int) = XillybusDevice(name, "mem", "write", bitWidth, addrWidth)
}

object XillybusMemRead {
  def apply(name: String, bitWidth: Int, addrWidth: Int) = XillybusDevice(name, "mem", "read", bitWidth, addrWidth)
}

object XillybusMemBi {
  def apply(name: String, bitWidth: Int, addrWidth: Int) = XillybusDevice(name, "mem", "bi", bitWidth, addrWidth)
}
