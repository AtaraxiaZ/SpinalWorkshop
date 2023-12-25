package workshop.pwm

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._

//APB configuration class (generic/parameter)
case class ApbConfig(addressWidth : Int,
                     dataWidth    : Int,
                     selWidth     : Int)

//APB interface definition
case class Apb(config: ApbConfig) extends Bundle with IMasterSlave {
  //TODO define APB signals
  import config._
  val PSEL = Bits(selWidth bits)
  val PENABLE = Bool()
  val PWRITE = Bool()
  val PADDR = UInt(addressWidth bits)
  val PWDATA = Bits(dataWidth bits)
  val PRDATA = Bits(dataWidth bits)
  val PREADY = Bool()

  override def asMaster(): Unit = {
    //TODO define direction of each signal in a master mode
    out(PSEL, PENABLE, PWRITE, PWDATA, PADDR)
    in(PRDATA, PREADY)
  }
}

case class ApbPwm(apbConfig: ApbConfig,timerWidth : Int) extends Component{
  require(apbConfig.dataWidth == 32)
  require(apbConfig.selWidth == 1)

  val io = new Bundle{
    val apb = slave(Apb(apbConfig)) //TODO
    val pwm = out Bool() //TODO
  }

  val counter_d = UInt(timerWidth bits)
  val counter_q = RegNext(counter_d) init(0) simPublic()

  // NOTE: Don't use 1 << timerWidth - 1. - have a higher priority than <<
  when(counter_q === ((1 << timerWidth) - 1)) {
    counter_d := 0
  } otherwise {
    counter_d := counter_q + 1
  }

  val control = new Area{
    //TODO define the APB slave logic that will make PWM's registers writable/readable
    val dutyCycle_d = UInt(timerWidth bits)
    val dutyCycle_q = RegNext(dutyCycle_d)
    val enable_d = Bits(1 bits)
    val enable_q = RegNext(enable_d)
    dutyCycle_d := dutyCycle_q
    enable_d := enable_q
    io.apb.PRDATA := 0

    when(io.apb.PSEL(0)) {
      when(io.apb.PENABLE) {
        when(io.apb.PWRITE) {
          when(io.apb.PADDR === 4) {
            dutyCycle_d := io.apb.PWDATA.asUInt.resized
          } elsewhen (io.apb.PADDR === 0) {
            enable_d := io.apb.PWDATA.resized
          }
        } otherwise {
          when(io.apb.PADDR === 4) {
            io.apb.PRDATA := dutyCycle_q.asBits.resized
          } elsewhen (io.apb.PADDR === 0) {
            io.apb.PRDATA := enable_q.resized
          }
        }
      }
    }
  }

  val logic = new Area {
    //TODO define the PWM logic
    val pwmReg = Reg(Bool())
    when(control.enable_q.asBool) {
      when(counter_q < control.dutyCycle_q) {
        io.pwm := True
      } otherwise {
        io.pwm := False
      }
    } otherwise {
      io.pwm := False
    }
  }


  io.apb.PREADY := True

}
