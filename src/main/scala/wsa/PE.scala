package wsa

import chisel3._

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

  // double buffered, shadow loads while active computes
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
