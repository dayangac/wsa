package wsa

import chisel3._

class PostProcess(n: Int, accWidth: Int = 32, dataWidth: Int = 8) extends Module {
  val io = IO(new Bundle {
    val in         = Input(Vec(n, SInt(accWidth.W)))
    val bias       = Input(Vec(n, SInt(accWidth.W)))
    val shift      = Input(UInt(5.W))
    val relu       = Input(Bool())
    val out        = Output(Vec(n, SInt(accWidth.W)))
    val outClamped = Output(Vec(n, SInt(dataWidth.W)))
  })

  val maxOut = ((1 << (dataWidth - 1)) - 1).S
  val minOut = (-(1 << (dataWidth - 1))).S
  // rounding shift, sum is 2 bits wider so it can't wrap
  val half   = ((1.U << io.shift) >> 1).asUInt.zext

  for (i <- 0 until n) {
    val sum     = io.in(i) +& io.bias(i) +& half
    val shifted = sum >> io.shift
    val v       = Mux(io.relu && shifted < 0.S, 0.S, shifted)
    io.out(i)        := v(accWidth - 1, 0).asSInt
    io.outClamped(i) := Mux(v > maxOut, maxOut, Mux(v < minOut, minOut, v))(dataWidth - 1, 0).asSInt
  }
}
