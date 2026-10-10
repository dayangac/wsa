package wsa

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

/** Byte encoding of the host protocol, the same layout as host/protocol.py. */
object Protocol {
  def u16(v: Int): Seq[Int] = Seq((v >> 8) & 0xff, v & 0xff)
  def s8(v: Int): Int       = v & 0xff
  def s32(v: Int): Seq[Int] = Seq(24, 16, 8, 0).map(s => (v >> s) & 0xff)

  def writeWeights(addr: Int, entries: Seq[Seq[Int]]) = Seq(Cmd.WriteWeights) ++ u16(addr) ++ u16(entries.length) ++ entries.flatten.map(s8)
  def writeActs(addr: Int, entries: Seq[Seq[Int]])    = Seq(Cmd.WriteActs) ++ u16(addr) ++ u16(entries.length) ++ entries.flatten.map(s8)
  def writeBias(addr: Int, entries: Seq[Seq[Int]])    = Seq(Cmd.WriteBias) ++ u16(addr) ++ u16(entries.length) ++ entries.flatten.flatMap(s32)
  def run(numLayers: Int)                             = Seq(Cmd.Run, numLayers)
  def readOut(addr: Int, count: Int)                  = Seq(Cmd.ReadOut) ++ u16(addr) ++ u16(count)
  val ping                                            = Seq(Cmd.Ping)

  def writeLayer(idx: Int, l: Layer, n: Int): Seq[Int] =
    Seq(Cmd.WriteLayer, idx) ++ u16(l.wBase) ++ u16(l.bBase) ++ u16(l.inBase) ++ u16(l.outBase) ++ u16(l.m) ++
      Seq(l.w.length / n, l.w.head.length / n, l.shift, (if (l.relu) 1 else 0) | (if (l.toOut) 2 else 0))

  /** Memory entries of a layer, in MatMulEngine's layout. */
  def weightEntries(w: Seq[Seq[Int]], n: Int): Seq[Seq[Int]] =
    for (nt <- 0 until w.head.length / n; kt <- 0 until w.length / n; r <- 0 until n)
      yield (0 until n).map(c => w(kt * n + r)(nt * n + c))
  def biasEntries(b: Seq[Int], n: Int): Seq[Seq[Int]] = b.grouped(n).toSeq
  def actEntries(x: Seq[Seq[Int]], n: Int): Seq[Seq[Int]] =
    for (kt <- 0 until x.head.length / n; row <- x.indices) yield x(row).slice(kt * n, (kt + 1) * n)

  /** Every command needed to load and run a network, then read the last layer's results. */
  def program(layers: Seq[Layer], n: Int): Seq[Int] = {
    val load = layers.zipWithIndex.flatMap { case (l, i) =>
      writeWeights(l.wBase, weightEntries(l.w, n)) ++ writeBias(l.bBase, biasEntries(l.b, n)) ++ writeLayer(i, l, n)
    }
    val last = layers.last
    load ++ writeActs(layers.head.inBase, actEntries(layers.head.x, n)) ++ run(layers.length) ++
      readOut(last.outBase, last.m * last.w.head.length / n)
  }

  /** Decodes a read reply into rows of int32 results, given the output layout. */
  def decodeOut(bytes: Seq[Int], m: Int, nTiles: Int, n: Int): Seq[Seq[BigInt]] = {
    val words = bytes.grouped(4).map(b => BigInt(b.foldLeft(0)((a, x) => (a << 8) | x))).toSeq
    val entries = words.grouped(n).toSeq
    (0 until m).map(row => (0 until nTiles).flatMap(nt => entries(nt * m + row)))
  }
}

class CommandParserSpec extends AnyFlatSpec with ChiselSim {
  behavior of "CommandParser with a Core"

  val n = 4

  /** Sends bytes with `gap` idle cycles between them, then keeps running until `expect` reply
    * bytes have come back. tx.ready is low on cycles where `stall` says so.
    */
  def exchange(dut: Accelerator, bytes: Seq[Int], expect: Int, gap: Int = 0, stall: Int => Boolean = _ => false): Seq[Int] = {
    val got   = Seq.newBuilder[Int]
    var count = 0
    var cycle = 0
    def tick(): Unit = {
      val ready = !stall(cycle)
      dut.io.tx.ready.poke(ready.B)
      if (ready && dut.io.tx.valid.peek().litToBoolean) {
        got += dut.io.tx.bits.peek().litValue.toInt
        count += 1
      }
      dut.clock.step()
      cycle += 1
    }
    for (b <- bytes) {
      dut.io.rx.valid.poke(true.B)
      dut.io.rx.bits.poke(b.U)
      tick()
      dut.io.rx.valid.poke(false.B)
      for (_ <- 0 until gap) tick()
    }
    var waited = 0
    while (count < expect) {
      tick()
      waited += 1
      assert(waited < 100000, s"only $count of $expect reply bytes")
    }
    for (_ <- 0 until 20) tick()
    assert(count == expect, s"$count reply bytes, expected $expect")
    got.result()
  }

  def mlp(rng: Random, m: Int): Seq[Layer] = Seq(
    Layer(Model.rand(rng, m, 8), Model.rand(rng, 8, 8), Seq.fill(8)(rng.between(-999, 999)), 7, relu = true, toOut = false,
      wBase = 0, bBase = 0, inBase = 0, outBase = 512),
    Layer(Seq.fill(m)(Seq.empty), Model.rand(rng, 8, 4), Seq.fill(4)(rng.between(-999, 999)), 2, relu = false, toOut = true,
      wBase = 16, bBase = 2, inBase = 512, outBase = 0)
  )

  def expected(layers: Seq[Layer]): Seq[Seq[BigInt]] = {
    val hidden = Model.layerValues(layers(0).x, layers(0).w, layers(0).b, layers(0).shift, relu = true)
      .map(_.map(v => PostProcessModel.clamp8(v).toInt))
    Model.layerValues(hidden, layers(1).w, layers(1).b, layers(1).shift, relu = false).map(_.map(PostProcessModel.wrap32))
  }

  def runProgram(dut: Accelerator, layers: Seq[Layer], gap: Int, stall: Int => Boolean = _ => false): Seq[Seq[BigInt]] = {
    val m     = layers.head.m
    val reply = exchange(dut, Protocol.program(layers, n), 1 + m * 4 * n, gap, stall)
    assert(reply.head == Cmd.RunDone)
    Protocol.decodeOut(reply.tail, m, 1, n).map(_.map(PostProcessModel.wrap32))
  }

  it should "answer a ping" in {
    simulate(new Accelerator(n)) { dut =>
      assert(exchange(dut, Protocol.ping, 1) == Seq(Cmd.Pong))
    }
  }

  it should "ignore unknown command bytes" in {
    simulate(new Accelerator(n)) { dut =>
      assert(exchange(dut, Seq(0x00, 0xff, 0x42) ++ Protocol.ping, 1) == Seq(Cmd.Pong))
    }
  }

  it should "reply done at once to a run of zero layers" in {
    simulate(new Accelerator(n)) { dut =>
      assert(exchange(dut, Protocol.run(0) ++ Protocol.ping, 2) == Seq(Cmd.RunDone, Cmd.Pong))
    }
  }

  it should "treat a write of zero entries as complete" in {
    simulate(new Accelerator(n)) { dut =>
      assert(exchange(dut, Protocol.writeWeights(0, Seq.empty) ++ Protocol.ping, 1) == Seq(Cmd.Pong))
    }
  }

  it should "load, run and read back a two layer network" in {
    val layers = mlp(new Random(1), m = 6)
    simulate(new Accelerator(n)) { dut =>
      assert(runProgram(dut, layers, gap = 3) == expected(layers))
    }
  }

  it should "keep up with a byte on every cycle and with tx stalls" in {
    val layers = mlp(new Random(2), m = 3)
    simulate(new Accelerator(n)) { dut =>
      val rng = new Random(0)
      val stalls = Seq.fill(100000)(rng.nextInt(3) == 0)
      assert(runProgram(dut, layers, gap = 0, stall = stalls) == expected(layers))
    }
  }

  it should "send int32 results most significant byte first" in {
    // One layer whose output is known: identity weights, bias -2, no shift.
    val l = Layer(Seq(Seq(1, 2, 3, 4)), Seq.tabulate(n, n)((r, c) => if (r == c) 1 else 0), Seq.fill(n)(-2), 0,
      relu = false, toOut = true)
    simulate(new Accelerator(n)) { dut =>
      val reply = exchange(dut, Protocol.program(Seq(l), n), 1 + 4 * n)
      assert(reply.tail == Seq(-1, 0, 1, 2).flatMap(Protocol.s32))
    }
  }
}
