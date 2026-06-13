package wsa

import chisel3._
import chisel3.util._

/** n x n weight-stationary systolic array computing y = x W.
  *
  * PE (r, c) holds W(r)(c), r being the input index and c the output index. Activations enter
  * on the left, row r taking x(r), and move right. Partial sums start at zero on the top edge
  * and move down, so the bottom of column c produces y(c) = sum over r of x(r) W(r)(c).
  *
  * The skew and deskew registers are inside the array: a whole input vector goes in on one
  * cycle and the whole output vector comes out `latency` cycles later, one vector per cycle.
  *
  * Loading: hold `shift` high for n cycles. Each column shifts down by one per cycle, so feed
  * row n - 1 first and row 0 last.
  */
class SystolicArray(val n: Int, dataWidth: Int = 8, accWidth: Int = 32) extends Module {
  val io = IO(new Bundle {
    val shift    = Input(Bool())
    val weightIn = Input(Vec(n, SInt(dataWidth.W)))
    val inValid  = Input(Bool())
    val actIn    = Input(Vec(n, SInt(dataWidth.W)))
    val outValid = Output(Bool())
    val out      = Output(Vec(n, SInt(accWidth.W)))
  })

  // Row r waits r cycles, so it meets the partial sum coming down from row r - 1.
  // Column c's sum leaves the bottom n + c cycles after its vector went in, and the deskew
  // delays it by n - 1 - c so all columns come out together.
  val latency = 2 * n - 1

  val pes = Seq.fill(n, n)(Module(new PE(dataWidth, accWidth)))

  for (r <- 0 until n; c <- 0 until n) {
    val pe = pes(r)(c)
    pe.io.shift   := io.shift
    pe.io.shiftIn := (if (r == 0) io.weightIn(c) else pes(r - 1)(c).io.shiftOut)
    pe.io.actIn   := (if (c == 0) ShiftRegister(io.actIn(r), r) else pes(r)(c - 1).io.actOut)
    pe.io.psumIn  := (if (r == 0) 0.S else pes(r - 1)(c).io.psumOut)
  }

  for (c <- 0 until n) {
    io.out(c) := ShiftRegister(pes(n - 1)(c).io.psumOut, n - 1 - c)
  }
  io.outValid := ShiftRegister(io.inValid, latency, false.B, true.B)
}
