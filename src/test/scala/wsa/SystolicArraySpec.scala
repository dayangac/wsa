package wsa

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class SystolicArraySpec extends AnyFlatSpec with ChiselSim {
  behavior of "SystolicArray"

  def matvec(w: Seq[Seq[Int]], x: Seq[Int]): Seq[BigInt] =
    w.head.indices.map(c => BigInt(x.indices.map(r => x(r) * w(r)(c)).sum))

  def randMatrix(rng: Random, n: Int) = Seq.fill(n, n)(rng.between(-128, 128))
  def randVec(rng: Random, n: Int)    = Seq.fill(n)(rng.between(-128, 128))

  case class Cycle(weightRow: Option[Seq[Int]] = None, act: Option[Seq[Int]] = None, swap: Boolean = false)

  case class Tile(w: Seq[Seq[Int]], inputs: Seq[Option[Seq[Int]]])

  def schedule(n: Int, tiles: Seq[Tile], loadGap: Int): Seq[Cycle] = {
    val cycles     = scala.collection.mutable.Map[Int, Cycle]().withDefaultValue(Cycle())
    var loadStart  = 0
    var streamFree = 0
    for (t <- tiles) {
      for (k <- 0 until n) {
        val c = loadStart + k
        cycles(c) = cycles(c).copy(weightRow = Some(t.w(n - 1 - k)))
      }
      val streamStart = math.max(loadStart + n, streamFree)
      t.inputs.zipWithIndex.foreach { case (x, i) =>
        val c = streamStart + i
        cycles(c) = cycles(c).copy(act = x, swap = i == 0)
      }
      streamFree = streamStart + t.inputs.length
      loadStart = math.max(streamStart + loadGap, loadStart + n)
    }
    (0 to cycles.keys.max).map(cycles)
  }

  def run(dut: SystolicArray, cycles: Seq[Cycle]): Seq[(Int, Seq[BigInt])] = {
    val outs = Seq.newBuilder[(Int, Seq[BigInt])]
    for ((cy, t) <- (cycles ++ Seq.fill(2 * dut.n + 2)(Cycle())).zipWithIndex) {
      dut.io.shift.poke(cy.weightRow.isDefined.B)
      cy.weightRow.foreach(_.zipWithIndex.foreach { case (w, c) => dut.io.weightIn(c).poke(w.S) })
      dut.io.inValid.poke(cy.act.isDefined.B)
      dut.io.swap.poke(cy.swap.B)
      cy.act.getOrElse(Seq.fill(dut.n)(0x5a)).zipWithIndex.foreach { case (a, r) => dut.io.actIn(r).poke(a.S) }
      if (dut.io.outValid.peek().litToBoolean) outs += ((t, dut.io.out.map(_.peek().litValue)))
      dut.clock.step()
    }
    outs.result()
  }

  def check(n: Int, tiles: Seq[Tile], loadGap: Int = -1): Unit =
    simulate(new SystolicArray(n)) { dut =>
      val gap    = if (loadGap < 0) dut.swapDistance else loadGap
      val cycles = schedule(n, tiles, gap)
      val outs   = run(dut, cycles)
      val want   = tiles.flatMap(t => t.inputs.flatten.map(matvec(t.w, _)))
      assert(outs.map(_._2) == want)
      assert(outs.head._1 - cycles.indexWhere(_.act.isDefined) == dut.latency)
    }

  it should "pass the input through identity weights" in {
    val n = 4
    check(n, Seq(Tile(Seq.tabulate(n, n)((r, c) => if (r == c) 1 else 0), Seq(Some(Seq(1, -2, 3, -4))))))
  }

  it should "route inputs through a permutation matrix" in {
    val n = 4
    check(n, Seq(Tile(Seq.tabulate(n, n)((r, c) => if (c == (r + 1) % n) 1 else 0), Seq(Some(Seq(10, 20, 30, 40))))))
  }

  for (n <- Seq(1, 2, 3, 4, 8)) {
    it should s"match a reference matmul for back-to-back vectors with n = $n" in {
      val rng = new Random(n)
      check(n, Seq(Tile(randMatrix(rng, n), Seq.fill(40)(Some(randVec(rng, n))))))
    }
  }

  it should "skip cycles where inValid is low" in {
    val rng    = new Random(7)
    val n      = 4
    val inputs = Seq.fill(20)(Some(randVec(rng, n))).flatMap(x => if (rng.nextBoolean()) Seq(x, None) else Seq(x))
    check(n, Seq(Tile(randMatrix(rng, n), Some(randVec(rng, n)) +: inputs)))
  }

  it should "not overflow with the int8 extremes" in {
    val n = 8
    check(n, Seq(Tile(Seq.fill(n, n)(-128), Seq(Some(Seq.fill(n)(-128)), Some(Seq.fill(n)(127))))))
  }

  for (n <- Seq(1, 2, 4, 5)) {
    it should s"load the next tile while streaming, with no gap between tiles (n = $n)" in {
      val rng = new Random(n)
      check(n, Seq(1, 3, 12).flatMap(m => Seq.fill(4)(Tile(randMatrix(rng, n), Seq.fill(m)(Some(randVec(rng, n)))))))
    }
  }

  it should "give wrong results if the next load starts before the swap reaches the last PE" in {
    val n   = 4
    val rng = new Random(11)
    val tiles = Seq.fill(2)(Tile(randMatrix(rng, n), Seq.fill(12)(Some(randVec(rng, n)))))
    simulate(new SystolicArray(n)) { dut =>
      val outs = run(dut, schedule(n, tiles, dut.swapDistance - 1))
      assert(outs.map(_._2) != tiles.flatMap(t => t.inputs.flatten.map(matvec(t.w, _))))
    }
  }
}
