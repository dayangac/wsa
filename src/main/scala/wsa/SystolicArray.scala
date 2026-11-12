package wsa

import chisel3._
import chisel3.util._

class SystolicArray(val n: Int, dataWidth: Int = 8, accWidth: Int = 32) extends Module {
  val io = IO(new Bundle {
    val shift    = Input(Bool())
    val weightIn = Input(Vec(n, SInt(dataWidth.W)))
    val inValid  = Input(Bool())
    val actIn    = Input(Vec(n, SInt(dataWidth.W)))
    val swap     = Input(Bool())
    val outValid = Output(Bool())
    val out      = Output(Vec(n, SInt(accWidth.W)))
  })

  val swapDistance = 2 * n - 2

  // skew rows in, deskew columns out so the whole vector comes out together
  val latency = 2 * n - 1

  val pes = Seq.fill(n, n)(Module(new PE(dataWidth, accWidth)))

  for (r <- 0 until n; c <- 0 until n) {
    val pe = pes(r)(c)
    pe.io.shift   := io.shift
    pe.io.shiftIn := (if (r == 0) io.weightIn(c) else pes(r - 1)(c).io.shiftOut)
    pe.io.actIn   := (if (c == 0) ShiftRegister(io.actIn(r), r) else pes(r)(c - 1).io.actOut)
    pe.io.swapIn  := (if (c == 0) ShiftRegister(io.swap, r, false.B, true.B) else pes(r)(c - 1).io.swapOut)
    pe.io.psumIn  := (if (r == 0) 0.S else pes(r - 1)(c).io.psumOut)
  }

  for (c <- 0 until n) {
    io.out(c) := ShiftRegister(pes(n - 1)(c).io.psumOut, n - 1 - c)
  }
  io.outValid := ShiftRegister(io.inValid, latency, false.B, true.B)
}
