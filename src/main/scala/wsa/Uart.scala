package wsa

import chisel3._
import chisel3.util._

class UartRx(clocksPerBit: Int) extends Module {
  require(clocksPerBit >= 2)
  val io = IO(new Bundle {
    val rxd = Input(Bool())
    val out = Valid(UInt(8.W))
  })

  // sync, line idles high
  val rx = RegNext(RegNext(io.rxd, true.B), true.B)

  object State extends ChiselEnum { val Idle, Start, Data, Stop = Value }
  val state = RegInit(State.Idle)
  val timer = Reg(UInt(log2Ceil(clocksPerBit + 1).W))
  val bit   = Reg(UInt(3.W))
  val shift = Reg(UInt(8.W))

  io.out.valid := false.B
  io.out.bits  := shift

  switch(state) {
    is(State.Idle) {
      when(!rx) {
        state := State.Start
        timer := (clocksPerBit / 2).U
      }
    }
    is(State.Start) {
      // check the start bit again mid bit
      when(timer === 0.U) {
        when(!rx) {
          state := State.Data
          timer := (clocksPerBit - 1).U
          bit   := 0.U
        }.otherwise { state := State.Idle }
      }.otherwise { timer := timer - 1.U }
    }
    is(State.Data) {
      when(timer === 0.U) {
        shift := Cat(rx, shift(7, 1))
        timer := (clocksPerBit - 1).U
        bit   := bit + 1.U
        when(bit === 7.U) { state := State.Stop }
      }.otherwise { timer := timer - 1.U }
    }
    is(State.Stop) {
      when(timer === 0.U) {
        // no stop bit, drop it
        io.out.valid := rx
        state        := State.Idle
      }.otherwise { timer := timer - 1.U }
    }
  }
}

class UartTx(clocksPerBit: Int) extends Module {
  require(clocksPerBit >= 1)
  val io = IO(new Bundle {
    val txd = Output(Bool())
    val in  = Flipped(Decoupled(UInt(8.W)))
  })

  // start, 8 data, stop
  val frame = RegInit(0x3ff.U(10.W))
  val left  = RegInit(0.U(4.W))
  val timer = Reg(UInt(log2Ceil(clocksPerBit).max(1).W))

  io.in.ready := left === 0.U
  io.txd      := frame(0)

  when(io.in.fire) {
    frame := Cat(1.U(1.W), io.in.bits, 0.U(1.W))
    left  := 10.U
    timer := (clocksPerBit - 1).U
  }.elsewhen(left =/= 0.U) {
    when(timer === 0.U) {
      frame := Cat(1.U(1.W), frame(9, 1))
      left  := left - 1.U
      timer := (clocksPerBit - 1).U
    }.otherwise { timer := timer - 1.U }
  }
}
