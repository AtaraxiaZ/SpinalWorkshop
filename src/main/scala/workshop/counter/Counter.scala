package workshop.counter

import spinal.core._

case class Counter(width: Int) extends Component {
  val io = new Bundle {
    val clear    = in  Bool()
    val value    = out UInt(width bits)
    val full     = out Bool()
  }

  val counter_d = UInt(width bits)
  val counter_q = RegNext(counter_d) init (0)

  //TODO define the logic
  when (io.clear) {
    counter_d := 0
  } otherwise {
    counter_d := counter_q + 1
  }

  io.full := counter_q === (1 << width) - 1 
  io.value := counter_q
}
