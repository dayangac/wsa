package wsa

import chisel3._

/** Weight-stationary processing element with a double-buffered weight.
  *
  * The PE multiplies the activation coming from the left by its weight, adds the partial sum
  * coming from above, and registers the result for the PE below. The activation is registered
  * and passed on to the right.
  *
  * There are two weight registers. `active` is the one being multiplied. `shadow` is loaded
  * while the PE works: while `shift` is high it takes `shiftIn` from the PE above, so a column
  * is a shift chain. The `swap` flag travels with the activations. The activation that carries
  * it is the first one multiplied by the shadow weight, which becomes the active weight from
  * then on.
  */
class PE(dataWidth: Int = 8, accWidth: Int = 32) extends Module {
  val io = IO(new Bundle {
    val shift    = Input(Bool())
    val shiftIn  = Input(SInt(dataWidth.W))
    val shiftOut = Output(SInt(dataWidth.W))
    val actIn    = Input(SInt(dataWidth.W))
    val swapIn   = Input(Bool())
    val actOut   = Output(SInt(dataWidth.W))
    val swapOut  = Output(Bool())
    val psumIn   = Input(SInt(accWidth.W))
    val psumOut  = Output(SInt(accWidth.W))
  })

  val shadow = RegInit(0.S(dataWidth.W))
  val active = RegInit(0.S(dataWidth.W))
  when(io.shift) { shadow := io.shiftIn }
  when(io.swapIn) { active := shadow }

  val weight = Mux(io.swapIn, shadow, active)

  io.shiftOut := shadow
  io.actOut   := RegNext(io.actIn, 0.S)
  io.swapOut  := RegNext(io.swapIn, false.B)
  io.psumOut  := RegNext(io.psumIn + weight * io.actIn, 0.S)
}
