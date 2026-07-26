package wsa

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class CoreSpec extends AnyFlatSpec with ChiselSim {
  behavior of "Core running one layer"

  def randomLayer(rng: Random, n: Int, m: Int, kTiles: Int, nTiles: Int, toOut: Boolean): Layer =
    Layer(
      x = Model.rand(rng, m, kTiles * n),
      w = Model.rand(rng, kTiles * n, nTiles * n),
      b = Seq.fill(nTiles * n)(rng.between(-65536, 65536)),
      shift = rng.between(0, 12),
      relu = rng.nextBoolean(),
      toOut = toOut
    )

  def check(n: Int, l: Layer): Int = {
    var cycles = 0
    simulate(new Core(n = n)) { dut =>
      val d          = new CoreDriver(dut)
      val (got, cyc) = d.runLayer(l)
      assert(got == d.expected(l))
      cycles = cyc
    }
    cycles
  }

  it should "compute a single tile" in {
    val n = 4
    val x = Seq(Seq(1, 2, 3, 4))
    val w = Seq.tabulate(n, n)((r, c) => if (r == c) 2 else 0)
    check(n, Layer(x, w, Seq(0, 1, 2, 3), shift = 0, relu = false, toOut = true))
  }

  for (n <- Seq(1, 2, 4); m <- Seq(1, 2, 5, 17); (kt, nt) <- Seq((1, 1), (3, 1), (1, 3), (2, 3))) {
    it should s"match the model with n = $n, M = $m, $kt x $nt tiles" in {
      val rng = new Random(n * 1000 + m * 10 + kt * 3 + nt)
      check(n, randomLayer(rng, n, m, kt, nt, toOut = rng.nextBoolean()))
    }
  }

  it should "write int8 results to the activation memory and int32 results to the output memory" in {
    val rng = new Random(5)
    for (toOut <- Seq(false, true)) check(4, randomLayer(rng, 4, 6, 2, 2, toOut))
  }

  it should "use the configured base addresses" in {
    val rng = new Random(9)
    check(4, randomLayer(rng, 4, 7, 2, 2, toOut = false).copy(wBase = 100, bBase = 17, inBase = 300, outBase = 1000))
  }

  it should "finish at once on an empty layer" in {
    simulate(new Core(n = 4)) { dut =>
      val d = new CoreDriver(dut)
      d.idle()
      val l = Layer(Seq.empty, Seq.fill(4, 4)(0), Seq.fill(4)(0), 0, relu = false, toOut = true)
      assert(d.run(l) == 0)
      dut.io.busy.expect(false.B)
    }
  }

  it should "hide weight loads behind streams when M is large enough" in {
    val n      = 4
    val m      = 32
    val tiles  = 3 * 3
    val rng    = new Random(21)
    val cycles = check(n, randomLayer(rng, n, m, 3, 3, toOut = true))
    // Each tile costs M cycles of streaming plus a couple of cycles of handover; only the first
    // load and the final drain are exposed.
    assert(cycles <= tiles * (m + 2) + n + 2 * n + 8, s"took $cycles cycles")
  }
}
