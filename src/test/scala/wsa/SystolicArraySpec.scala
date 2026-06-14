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

  def loadWeights(dut: SystolicArray, w: Seq[Seq[Int]]): Unit = {
    dut.io.shift.poke(true.B)
    for (r <- (dut.n - 1) to 0 by -1) {
      for (c <- 0 until dut.n) dut.io.weightIn(c).poke(w(r)(c).S)
      dut.clock.step()
    }
    dut.io.shift.poke(false.B)
  }

  /** Streams the inputs (None is a cycle with inValid low) and returns every valid output with
    * the number of cycles since the first input.
    */
  def stream(dut: SystolicArray, inputs: Seq[Option[Seq[Int]]]): Seq[(Int, Seq[BigInt])] = {
    val outs  = Seq.newBuilder[(Int, Seq[BigInt])]
    var cycle = 0
    def sample(): Unit =
      if (dut.io.outValid.peek().litToBoolean)
        outs += ((cycle, dut.io.out.map(_.peek().litValue)))
    for (x <- inputs) {
      x match {
        case Some(v) =>
          dut.io.inValid.poke(true.B)
          v.zipWithIndex.foreach { case (a, r) => dut.io.actIn(r).poke(a.S) }
        case None =>
          dut.io.inValid.poke(false.B)
          dut.io.actIn.foreach(_.poke(0x5a.S))
      }
      sample()
      dut.clock.step()
      cycle += 1
    }
    dut.io.inValid.poke(false.B)
    for (_ <- 0 until 2 * dut.n + 2) {
      sample()
      dut.clock.step()
      cycle += 1
    }
    outs.result()
  }

  def check(n: Int, w: Seq[Seq[Int]], inputs: Seq[Option[Seq[Int]]]): Unit =
    simulate(new SystolicArray(n)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      loadWeights(dut, w)
      val outs = stream(dut, inputs)
      assert(outs.map(_._2) == inputs.flatten.map(matvec(w, _)))
      assert(outs.head._1 - inputs.indexWhere(_.isDefined) == dut.latency)
    }

  it should "pass the input through identity weights" in {
    val n = 4
    check(n, Seq.tabulate(n, n)((r, c) => if (r == c) 1 else 0), Seq(Some(Seq(1, -2, 3, -4))))
  }

  it should "route inputs through a permutation matrix" in {
    val n    = 4
    val perm = Seq.tabulate(n, n)((r, c) => if (c == (r + 1) % n) 1 else 0)
    simulate(new SystolicArray(n)) { dut =>
      loadWeights(dut, perm)
      val outs = stream(dut, Seq(Some(Seq(10, 20, 30, 40))))
      assert(outs.map(_._2) == Seq(Seq(40, 10, 20, 30).map(BigInt(_))))
    }
  }

  for (n <- Seq(1, 2, 3, 4, 8)) {
    it should s"match a reference matmul for back-to-back vectors with n = $n" in {
      val rng = new Random(n)
      check(n, randMatrix(rng, n), Seq.fill(40)(Some(randVec(rng, n))))
    }
  }

  it should "skip cycles where inValid is low" in {
    val rng    = new Random(7)
    val n      = 4
    val inputs = Seq.fill(20)(Some(randVec(rng, n))).flatMap(x => if (rng.nextBoolean()) Seq(x, None) else Seq(x))
    check(n, randMatrix(rng, n), None +: inputs)
  }

  it should "not overflow with the int8 extremes" in {
    val n = 8
    check(n, Seq.fill(n, n)(-128), Seq(Some(Seq.fill(n)(-128)), Some(Seq.fill(n)(127))))
  }
}
