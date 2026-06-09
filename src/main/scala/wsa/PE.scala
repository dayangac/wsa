package wsa

import chisel3._

/** Weight-stationary processing element.
  *
  * The PE holds one weight. Each cycle it multiplies the activation coming from the left by
  * that weight, adds the partial sum coming from above, and registers the result for the PE
  * below. The activation is registered and passed on to the right.
  *
  * While `shift` is high the weight register takes `shiftIn`, which comes from the PE above,
  * so a column of PEs is a shift chain for loading weights.
  */
class PE(dataWidth: Int = 8, accWidth: Int = 32) extends Module {
  val io = IO(new Bundle {
    val shift    = Input(Bool())
    val shiftIn  = Input(SInt(dataWidth.W))
    val shiftOut = Output(SInt(dataWidth.W))
    val actIn    = Input(SInt(dataWidth.W))
    val actOut   = Output(SInt(dataWidth.W))
    val psumIn   = Input(SInt(accWidth.W))
    val psumOut  = Output(SInt(accWidth.W))
  })

  val weight = RegInit(0.S(dataWidth.W))
  when(io.shift) { weight := io.shiftIn }

  io.shiftOut := weight
  io.actOut   := RegNext(io.actIn, 0.S)
  io.psumOut  := RegNext(io.psumIn + weight * io.actIn, 0.S)
}
