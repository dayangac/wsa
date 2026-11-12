package wsa

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

object PostProcessModel {
  def value(acc: BigInt, bias: BigInt, shift: Int, relu: Boolean): BigInt = {
    val half = (BigInt(1) << shift) >> 1
    val v    = (acc + bias + half) >> shift
    if (relu && v < 0) 0 else v
  }
  def wrap32(v: BigInt): BigInt  = { val m = v & 0xffffffffL; if (m >= (BigInt(1) << 31)) m - (BigInt(1) << 32) else m }
  def clamp8(v: BigInt): BigInt  = v.max(-128).min(127)
}

class PostProcessSpec extends AnyFlatSpec with ChiselSim {
  import PostProcessModel._
  behavior of "PostProcess"

  val n = 4

  def check(dut: PostProcess, acc: Seq[BigInt], bias: Seq[BigInt], shift: Int, relu: Boolean): Unit = {
    for (i <- 0 until n) {
      dut.io.in(i).poke(acc(i).S)
      dut.io.bias(i).poke(bias(i).S)
    }
    dut.io.shift.poke(shift.U)
    dut.io.relu.poke(relu.B)
    for (i <- 0 until n) {
      val v = value(acc(i), bias(i), shift, relu)
      dut.io.out(i).expect(wrap32(v).S, s"lane $i out, acc ${acc(i)} bias ${bias(i)} shift $shift relu $relu")
      dut.io.outClamped(i).expect(clamp8(v).S, s"lane $i clamped, acc ${acc(i)} bias ${bias(i)} shift $shift relu $relu")
    }
  }

  it should "round half up when shifting" in {
    simulate(new PostProcess(n)) { dut =>
      check(dut, Seq(5, 6, 7, -5).map(BigInt(_)), Seq.fill(n)(BigInt(0)), 1, relu = false)
      for (i <- 0 until n) dut.io.out(i).expect(Seq(3, 3, 4, -2)(i).S)
      check(dut, Seq(-3, -1, 1, 3).map(BigInt(_)), Seq.fill(n)(BigInt(0)), 2, relu = false)
      for (i <- 0 until n) dut.io.out(i).expect(Seq(-1, 0, 0, 1)(i).S)
    }
  }

  it should "add the bias before shifting and leave values alone with shift 0" in {
    simulate(new PostProcess(n)) { dut =>
      check(dut, Seq(10, -10, 0, 100).map(BigInt(_)), Seq(1, 2, -3, 28).map(BigInt(_)), 0, relu = false)
      for (i <- 0 until n) dut.io.out(i).expect(Seq(11, -8, -3, 128)(i).S)
      for (i <- 0 until n) dut.io.outClamped(i).expect(Seq(11, -8, -3, 127)(i).S)
    }
  }

  it should "zero negative values with relu" in {
    simulate(new PostProcess(n)) { dut =>
      check(dut, Seq(-1, 0, 1, -1000).map(BigInt(_)), Seq.fill(n)(BigInt(0)), 0, relu = true)
      for (i <- 0 until n) dut.io.out(i).expect(Seq(0, 0, 1, 0)(i).S)
    }
  }

  it should "saturate to int8 at both ends" in {
    simulate(new PostProcess(n)) { dut =>
      check(dut, Seq(127, 128, -128, -129).map(BigInt(_)), Seq.fill(n)(BigInt(0)), 0, relu = false)
      for (i <- 0 until n) dut.io.outClamped(i).expect(Seq(127, 127, -128, -128)(i).S)
    }
  }

  it should "not wrap when acc + bias exceeds 32 bits before the shift" in {
    simulate(new PostProcess(n)) { dut =>
      val big = BigInt(Int.MaxValue)
      check(dut, Seq(big, -big - 1, big, 0), Seq(big, -big - 1, 1, 0), 8, relu = false)
    }
  }

  it should "match the model for random inputs, shifts and relu" in {
    val rng = new Random(3)
    def r32 = BigInt(rng.nextInt())
    simulate(new PostProcess(n)) { dut =>
      for (_ <- 0 until 500) {
        check(dut, Seq.fill(n)(r32 >> rng.between(0, 24)), Seq.fill(n)(r32 >> 16), rng.between(0, 32), rng.nextBoolean())
      }
    }
  }
}
