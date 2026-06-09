package wsa

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec

class PESpec extends AnyFlatSpec with ChiselSim {
  behavior of "PE"

  def load(dut: PE, w: Int): Unit = {
    dut.io.shift.poke(true.B)
    dut.io.shiftIn.poke(w.S)
    dut.clock.step()
    dut.io.shift.poke(false.B)
  }

  it should "add weight times activation to the partial sum a cycle later" in {
    simulate(new PE) { dut =>
      load(dut, -3)
      for ((a, p) <- Seq((5, 100), (-128, 0), (127, -7), (0, 42))) {
        dut.io.actIn.poke(a.S)
        dut.io.psumIn.poke(p.S)
        dut.clock.step()
        dut.io.psumOut.expect((p - 3 * a).S)
        dut.io.actOut.expect(a.S)
      }
    }
  }

  it should "keep its weight while shift is low" in {
    simulate(new PE) { dut =>
      load(dut, 7)
      dut.io.shiftIn.poke(-1.S)
      dut.clock.step(5)
      dut.io.shiftOut.expect(7.S)
      dut.io.actIn.poke(2.S)
      dut.io.psumIn.poke(0.S)
      dut.clock.step()
      dut.io.psumOut.expect(14.S)
    }
  }

  it should "multiply the int8 extremes without overflow" in {
    simulate(new PE) { dut =>
      load(dut, -128)
      dut.io.actIn.poke(-128.S)
      dut.io.psumIn.poke(0.S)
      dut.clock.step()
      dut.io.psumOut.expect(16384.S)
    }
  }
}
