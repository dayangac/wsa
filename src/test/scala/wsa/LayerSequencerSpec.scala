package wsa

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class LayerSequencerSpec extends AnyFlatSpec with ChiselSim {
  behavior of "Core running a network"

  /** A random MLP with the given widths. Layers ping-pong between activation regions 0 and
    * 512; the last one writes int32 results to the output memory.
    */
  def network(rng: Random, m: Int, widths: Seq[Int], shift: Int = 8): Seq[Layer] = {
    val x     = Model.rand(rng, m, widths.head)
    var wBase = 0
    var bBase = 0
    widths.sliding(2).zipWithIndex.map { case (Seq(k, nOut), i) =>
      val last = i == widths.length - 2
      val l = Layer(
        x = if (i == 0) x else Seq.fill(m)(Seq.empty), // only the row count matters after layer 0
        w = Model.rand(rng, k, nOut),
        b = Seq.fill(nOut)(rng.between(-4096, 4096)),
        shift = shift,
        relu = !last,
        toOut = last,
        wBase = wBase,
        bBase = bBase,
        inBase = if (i % 2 == 0) 0 else 512,
        outBase = if (last) 0 else if (i % 2 == 0) 512 else 0
      )
      wBase += k * nOut / 4
      bBase += nOut / 4
      l
    }.toSeq
  }

  it should "chain three layers through the activation memory" in {
    val layers = network(new Random(1), m = 9, widths = Seq(16, 12, 8, 4))
    simulate(new Core(n = 4)) { dut =>
      val d = new CoreDriver(dut)
      assert(d.runNetwork(layers) == d.expectedNetwork(layers))
    }
  }

  it should "produce a mix of clamped and unclamped values between layers" in {
    // Guards the test above against a network whose hidden values are all 0 or all saturated.
    val layers = network(new Random(1), m = 9, widths = Seq(16, 12, 8, 4))
    val hidden = Model.layerValues(layers.head.x, layers.head.w, layers.head.b, layers.head.shift, relu = true).flatten
    assert(hidden.count(v => v > 0 && v < 127) > hidden.length / 4)
  }

  it should "run a single-row batch through a deep narrow network" in {
    val layers = network(new Random(2), m = 1, widths = Seq(4, 4, 4, 4, 4, 4, 4), shift = 6)
    simulate(new Core(n = 4)) { dut =>
      val d = new CoreDriver(dut)
      assert(d.runNetwork(layers) == d.expectedNetwork(layers))
    }
  }

  it should "give the same results when started again" in {
    val layers = network(new Random(3), m = 5, widths = Seq(8, 8, 4))
    simulate(new Core(n = 4)) { dut =>
      val d     = new CoreDriver(dut)
      val first = d.runNetwork(layers)
      d.writeActs(0, layers.head.x)
      d.run(layers.length)
      assert(d.readOut(0, 5, 1) == first)
      assert(first == d.expectedNetwork(layers))
    }
  }

  it should "run only the first numLayers entries of the table" in {
    val rng    = new Random(4)
    val layers = network(rng, m = 3, widths = Seq(8, 4))
    simulate(new Core(n = 4)) { dut =>
      val d = new CoreDriver(dut)
      // A second entry that would overwrite the output memory with zeros if it ran.
      d.idle()
      d.writeLayer(1, layers.head.copy(w = Seq.fill(8, 4)(0), b = Seq.fill(4)(0), wBase = 2000, bBase = 100))
      assert(d.runNetwork(layers) == d.expectedNetwork(layers))
    }
  }

  it should "finish at once with zero layers" in {
    simulate(new Core(n = 4)) { dut =>
      val d = new CoreDriver(dut)
      d.idle()
      assert(d.run(0) == 0)
      dut.io.busy.expect(false.B)
    }
  }
}

class LayerSequencerUnitSpec extends AnyFlatSpec with ChiselSim {
  behavior of "LayerSequencer"

  /** Starts a run and answers every engine start with done a few cycles later. Returns how many
    * layers were started before the sequencer reported done.
    */
  def countLayers(dut: LayerSequencer, numLayers: Int): Int = {
    dut.io.tableWrEn.poke(false.B)
    dut.io.engDone.poke(false.B)
    dut.io.numLayers.poke(numLayers.U)
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    var starts = 0
    var cycles = 0
    var done   = false
    while (!done) {
      if (dut.io.engStart.peek().litToBoolean) {
        starts += 1
        dut.clock.step(3)
        dut.io.engDone.poke(true.B)
      }
      done = dut.io.done.peek().litToBoolean
      dut.clock.step()
      dut.io.engDone.poke(false.B)
      cycles += 1
      assert(cycles < 1000, "sequencer did not finish")
    }
    starts
  }

  it should "run as many layers as asked" in {
    simulate(new LayerSequencer(16)) { dut =>
      assert(countLayers(dut, 5) == 5)
    }
  }

  it should "stop at the size of the table when asked for more layers" in {
    simulate(new LayerSequencer(16)) { dut =>
      assert(countLayers(dut, 20) == 16)
    }
  }
}
