package wsa

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec

class TopSpec extends AnyFlatSpec with ChiselSim {
  behavior of "Top"

  val clocksPerBit = 4

  class Host(dut: Top) {
    private val got     = Seq.newBuilder[Int]
    private var count   = 0
    private var started = -1 // -1 when idle
    private var bits    = 0
    private var cycle   = 0
    private var lastTxd = true

    def received: Seq[Int] = got.result()

    def tick(): Unit = {
      val txd = dut.io.txd.peek().litToBoolean
      if (started < 0) {
        if (lastTxd && !txd) { started = cycle; bits = 0 }
      } else {
        // sample mid bit, bit 0 is the start bit
        val t = cycle - started
        if (t % clocksPerBit == clocksPerBit / 2) {
          val i = t / clocksPerBit
          if (i >= 1 && i <= 8 && txd) bits |= 1 << (i - 1)
          if (i == 9) {
            assert(txd, "missing stop bit")
            got += bits
            count += 1
            started = -1
          }
        }
      }
      lastTxd = txd
      dut.clock.step()
      cycle += 1
    }

    def send(bytes: Seq[Int]): Unit =
      for (b <- bytes; level <- UartLine.frame(b); _ <- 0 until clocksPerBit) {
        dut.io.rxd.poke(level.B)
        tick()
      }

    def waitFor(n: Int, timeout: Int = 2000000): Unit = {
      dut.io.rxd.poke(true.B)
      var waited = 0
      while (count < n) {
        tick()
        waited += 1
        assert(waited < timeout, s"got $count of $n reply bytes")
      }
    }
  }

  def withTop(body: (Top, Host) => Unit): Unit =
    simulate(new Top(4, clocksPerBit)) { dut =>
      dut.io.rxd.poke(true.B)
      dut.clock.step(10)
      body(dut, new Host(dut))
    }

  it should "answer a ping over the UART" in {
    withTop { (_, host) =>
      host.send(Protocol.ping)
      host.waitFor(1)
      assert(host.received == Seq(Cmd.Pong))
    }
  }

  it should "classify digits with the logits the python model predicts" in {
    val program = getClass.getResourceAsStream("/digits/program.bin").readAllBytes().map(_ & 0xff).toSeq
    val lines   = scala.io.Source.fromResource("digits/expected.txt").getLines().toSeq
    val Array(m, lanes, replyLen) = lines.head.split(" ").map(_.toInt)
    val rows    = lines.tail.map(_.split(" ").map(_.toInt).toSeq)
    val labels  = rows.map(_.head)
    val want    = rows.map(_.tail.map(BigInt(_)))

    withTop { (dut, host) =>
      host.send(program)
      host.waitFor(replyLen)
      val reply = host.received
      assert(reply.head == Cmd.RunDone)
      val got = Protocol.decodeOut(reply.tail, m, lanes / 4, 4).map(_.map(PostProcessModel.wrap32))
      assert(got == want)
      val right = got.zip(labels).count { case (l, y) => l.take(10).indexOf(l.take(10).max) == y }
      info(s"$right of $m digits right")
      dut.io.busy.expect(false.B)
    }
  }
}
