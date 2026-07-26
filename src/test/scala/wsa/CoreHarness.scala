package wsa

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import scala.util.Random

/** Integer reference of a layer, on matrices already padded to multiples of n. */
object Model {
  def layerValues(x: Seq[Seq[Int]], w: Seq[Seq[Int]], b: Seq[Int], shift: Int, relu: Boolean): Seq[Seq[BigInt]] =
    x.map { row =>
      w.head.indices.map { c =>
        val acc = BigInt(row.indices.map(k => row(k) * w(k)(c)).sum)
        PostProcessModel.value(acc, BigInt(b(c)), shift, relu)
      }
    }

  def rand(rng: Random, rows: Int, cols: Int, lo: Int = -128, hi: Int = 128): Seq[Seq[Int]] =
    Seq.fill(rows, cols)(rng.between(lo, hi))
}

/** One layer to run on the core: matrices padded to multiples of n, and where they live. */
case class Layer(
  x:       Seq[Seq[Int]],
  w:       Seq[Seq[Int]],
  b:       Seq[Int],
  shift:   Int,
  relu:    Boolean,
  toOut:   Boolean,
  wBase:   Int = 0,
  bBase:   Int = 0,
  inBase:  Int = 0,
  outBase: Int = 512) {
  def m = x.length
}

/** Drives a Core through its ports using the memory layout of MatMulEngine. */
class CoreDriver(dut: Core) {
  val n = dut.n

  def idle(): Unit = {
    Seq(dut.io.wWrEn, dut.io.bWrEn, dut.io.aWrEn, dut.io.start).foreach(_.poke(false.B))
  }

  def writeWeights(base: Int, w: Seq[Seq[Int]]): Unit = {
    val kTiles = w.length / n
    val nTiles = w.head.length / n
    for (nt <- 0 until nTiles; kt <- 0 until kTiles; r <- 0 until n) {
      dut.io.wWrEn.poke(true.B)
      dut.io.wWrAddr.poke((base + (nt * kTiles + kt) * n + r).U)
      for (c <- 0 until n) dut.io.wWrData(c).poke(w(kt * n + r)(nt * n + c).S)
      dut.clock.step()
    }
    dut.io.wWrEn.poke(false.B)
  }

  def writeBias(base: Int, b: Seq[Int]): Unit = {
    for (nt <- 0 until b.length / n) {
      dut.io.bWrEn.poke(true.B)
      dut.io.bWrAddr.poke((base + nt).U)
      for (c <- 0 until n) dut.io.bWrData(c).poke(b(nt * n + c).S)
      dut.clock.step()
    }
    dut.io.bWrEn.poke(false.B)
  }

  def writeActs(base: Int, x: Seq[Seq[Int]]): Unit = {
    val m = x.length
    for (kt <- 0 until x.head.length / n; row <- 0 until m) {
      dut.io.aWrEn.poke(true.B)
      dut.io.aWrAddr.poke((base + kt * m + row).U)
      for (c <- 0 until n) dut.io.aWrData(c).poke(x(row)(kt * n + c).S)
      dut.clock.step()
    }
    dut.io.aWrEn.poke(false.B)
  }

  def pokeConfig(l: Layer): Unit = {
    val c = dut.io.cfg
    c.wBase.poke(l.wBase.U)
    c.bBase.poke(l.bBase.U)
    c.inBase.poke(l.inBase.U)
    c.outBase.poke(l.outBase.U)
    c.m.poke(l.m.U)
    c.kTiles.poke((l.w.length / n).U)
    c.nTiles.poke((l.w.head.length / n).U)
    c.shift.poke(l.shift.U)
    c.relu.poke(l.relu.B)
    c.toOut.poke(l.toOut.B)
  }

  /** Pulses start and waits for done. Returns the cycles from start to done. */
  def run(l: Layer, timeout: Int = 100000): Int = {
    pokeConfig(l)
    dut.io.start.poke(true.B)
    var cycles = 0
    var done   = dut.io.done.peek().litToBoolean
    dut.clock.step()
    dut.io.start.poke(false.B)
    while (!done) {
      cycles += 1
      assert(cycles < timeout, "engine did not finish")
      done = dut.io.done.peek().litToBoolean
      dut.clock.step()
    }
    cycles
  }

  def readOut(base: Int, m: Int, nTiles: Int): Seq[Seq[BigInt]] =
    (0 until m).map { row =>
      (0 until nTiles).flatMap { nt =>
        dut.io.outRdAddr.poke((base + nt * m + row).U)
        dut.clock.step()
        dut.io.outRdData.map(_.peek().litValue)
      }
    }

  def readActs(base: Int, m: Int, tiles: Int): Seq[Seq[BigInt]] =
    (0 until m).map { row =>
      (0 until tiles).flatMap { t =>
        dut.io.aRdAddr.poke((base + t * m + row).U)
        dut.clock.step()
        dut.io.aRdData.map(_.peek().litValue)
      }
    }

  /** Loads a layer, runs it and returns (results, cycles). */
  def runLayer(l: Layer): (Seq[Seq[BigInt]], Int) = {
    idle()
    writeWeights(l.wBase, l.w)
    writeBias(l.bBase, l.b)
    writeActs(l.inBase, l.x)
    val cycles = run(l)
    val nTiles = l.w.head.length / n
    val got    = if (l.toOut) readOut(l.outBase, l.m, nTiles) else readActs(l.outBase, l.m, nTiles)
    (got, cycles)
  }

  def expected(l: Layer): Seq[Seq[BigInt]] =
    Model.layerValues(l.x, l.w, l.b, l.shift, l.relu).map(_.map { v =>
      if (l.toOut) PostProcessModel.wrap32(v) else PostProcessModel.clamp8(v)
    })
}
