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

  def mac(dut: PE, a: Int, p: Int, swap: Boolean = false): BigInt = {
    dut.io.actIn.poke(a.S)
    dut.io.psumIn.poke(p.S)
    dut.io.swapIn.poke(swap.B)
    dut.clock.step()
    dut.io.swapIn.poke(false.B)
    dut.io.psumOut.peek().litValue
  }

  it should "add weight times activation to the partial sum a cycle later" in {
    simulate(new PE) { dut =>
      load(dut, -3)
      assert(mac(dut, 5, 100, swap = true) == 100 - 15)
      for ((a, p) <- Seq((-128, 0), (127, -7), (0, 42))) {
        assert(mac(dut, a, p) == p - 3 * a)
        dut.io.actOut.expect(a.S)
      }
    }
  }

  it should "use the shadow weight only from the activation that carries swap" in {
    simulate(new PE) { dut =>
      load(dut, 2)
      assert(mac(dut, 1, 0, swap = true) == 2)
      load(dut, 5)
      assert(mac(dut, 1, 0) == 2)
      assert(mac(dut, 1, 0, swap = true) == 5)
      assert(mac(dut, 1, 0) == 5)
    }
  }

  it should "use the old shadow value when shift and swap happen on the same cycle" in {
    simulate(new PE) { dut =>
      load(dut, 3)
      dut.io.shift.poke(true.B)
      dut.io.shiftIn.poke(9.S)
      assert(mac(dut, 1, 0, swap = true) == 3)
      dut.io.shift.poke(false.B)
      assert(mac(dut, 1, 0) == 3)
      dut.io.shiftOut.expect(9.S)
    }
  }

  it should "pass swap to the right a cycle later" in {
    simulate(new PE) { dut =>
      mac(dut, 0, 0, swap = true)
      dut.io.swapOut.expect(true.B)
      mac(dut, 0, 0)
      dut.io.swapOut.expect(false.B)
    }
  }

  it should "multiply the int8 extremes without overflow" in {
    simulate(new PE) { dut =>
      load(dut, -128)
      assert(mac(dut, -128, 0, swap = true) == 16384)
    }
  }
}
