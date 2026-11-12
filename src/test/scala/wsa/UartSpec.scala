package wsa

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

object UartLine {
  def frame(byte: Int): Seq[Boolean] = false +: (0 until 8).map(i => ((byte >> i) & 1) == 1) :+ true
}

class UartSpec extends AnyFlatSpec with ChiselSim {
  val clocksPerBit = 8

  class Loopback extends Module {
    val io = IO(new Bundle {
      val in  = Flipped(chisel3.util.Decoupled(UInt(8.W)))
      val out = chisel3.util.Valid(UInt(8.W))
    })
    val tx = Module(new UartTx(clocksPerBit))
    val rx = Module(new UartRx(clocksPerBit))
    tx.io.in <> io.in
    rx.io.rxd := tx.io.txd
    io.out    := rx.io.out
  }

  behavior of "UartRx"

  def receive(dut: UartRx, bits: Seq[Boolean], bitTime: Int): Seq[Int] = {
    val got = Seq.newBuilder[Int]
    for (b <- bits ++ Seq.fill(4)(true); _ <- 0 until bitTime) {
      dut.io.rxd.poke(b.B)
      dut.clock.step()
      if (dut.io.out.valid.peek().litToBoolean) got += dut.io.out.bits.peek().litValue.toInt
    }
    got.result()
  }

  it should "receive back-to-back bytes" in {
    val bytes = Seq(0x00, 0xff, 0x55, 0xaa, 0x01, 0x80, 0x3c)
    simulate(new UartRx(clocksPerBit)) { dut =>
      dut.io.rxd.poke(true.B)
      dut.clock.step(5)
      assert(receive(dut, bytes.flatMap(UartLine.frame), clocksPerBit) == bytes)
    }
  }

  it should "tolerate a sender 4% fast or slow" in {
    val bytes = Seq.tabulate(20)(i => (i * 37) & 0xff)
    for (bitTime <- Seq(25, 26, 27)) {
      simulate(new UartRx(26)) { dut =>
        dut.io.rxd.poke(true.B)
        dut.clock.step(5)
        assert(receive(dut, bytes.flatMap(UartLine.frame), bitTime) == bytes, s"bit time $bitTime")
      }
    }
  }

  it should "ignore a glitch shorter than half a bit" in {
    simulate(new UartRx(clocksPerBit)) { dut =>
      dut.io.rxd.poke(true.B)
      dut.clock.step(5)
      val glitch = Seq.fill(2)(false) ++ Seq.fill(30)(true)
      val got    = Seq.newBuilder[Int]
      for (b <- glitch) {
        dut.io.rxd.poke(b.B)
        dut.clock.step()
        if (dut.io.out.valid.peek().litToBoolean) got += 1
      }
      assert(got.result().isEmpty)
      assert(receive(dut, UartLine.frame(0x42), clocksPerBit) == Seq(0x42))
    }
  }

  it should "drop a byte whose stop bit is low" in {
    simulate(new UartRx(clocksPerBit)) { dut =>
      dut.io.rxd.poke(true.B)
      dut.clock.step(5)
      val broken = UartLine.frame(0x42).init :+ false
      assert(receive(dut, broken ++ Seq.fill(12)(true) ++ UartLine.frame(0x17), clocksPerBit) == Seq(0x17))
    }
  }

  behavior of "UartTx"

  it should "send idle high, then start, LSB first data and stop bits" in {
    simulate(new UartTx(clocksPerBit)) { dut =>
      dut.io.txd.expect(true.B)
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.poke(0xa6.U)
      dut.clock.step()
      dut.io.in.valid.poke(false.B)
      val line = (0 until 10 * clocksPerBit).map { _ =>
        val b = dut.io.txd.peek().litToBoolean
        dut.clock.step()
        b
      }
      assert(line.grouped(clocksPerBit).map(_.distinct).toSeq == UartLine.frame(0xa6).map(Seq(_)))
      dut.io.txd.expect(true.B)
      dut.io.in.ready.expect(true.B)
    }
  }

  it should "not accept a byte while sending" in {
    simulate(new UartTx(clocksPerBit)) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.poke(1.U)
      dut.clock.step()
      for (_ <- 0 until 10 * clocksPerBit) {
        dut.io.in.ready.expect(false.B)
        dut.clock.step()
      }
      dut.io.in.ready.expect(true.B)
    }
  }

  behavior of "UartTx into UartRx"

  it should "carry random bytes sent as fast as the transmitter allows" in {
    val bytes = Seq.fill(64)(new Random(8).nextInt(256)).zipWithIndex.map { case (b, i) => (b + i * 7) & 0xff }
    simulate(new Loopback) { dut =>
      val got  = Seq.newBuilder[Int]
      var left = bytes
      for (_ <- 0 until (bytes.length + 2) * 10 * clocksPerBit) {
        dut.io.in.valid.poke(left.nonEmpty.B)
        dut.io.in.bits.poke(left.headOption.getOrElse(0).U)
        if (left.nonEmpty && dut.io.in.ready.peek().litToBoolean) left = left.tail
        if (dut.io.out.valid.peek().litToBoolean) got += dut.io.out.bits.peek().litValue.toInt
        dut.clock.step()
      }
      assert(got.result() == bytes)
    }
  }
}
