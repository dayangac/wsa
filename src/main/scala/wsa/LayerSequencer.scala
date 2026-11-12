package wsa

import chisel3._
import chisel3.util._

class LayerSequencer(maxLayers: Int = 16) extends Module {
  val io = IO(new Bundle {
    val tableWrEn   = Input(Bool())
    val tableWrIdx  = Input(UInt(log2Ceil(maxLayers).W))
    val tableWrData = Input(new LayerConfig)

    val start     = Input(Bool())
    val numLayers = Input(UInt(8.W))
    val busy      = Output(Bool())
    val done      = Output(Bool())

    val engStart = Output(Bool())
    val engCfg   = Output(new LayerConfig)
    val engDone  = Input(Bool())
  })

  val table = Reg(Vec(maxLayers, new LayerConfig))
  when(io.tableWrEn) { table(io.tableWrIdx) := io.tableWrData }

  object State extends ChiselEnum { val Idle, Launch, Wait = Value }
  val state = RegInit(State.Idle)
  val idx   = Reg(UInt(8.W))
  val num   = Reg(UInt(8.W))

  io.busy     := state =/= State.Idle
  io.done     := false.B
  io.engStart := state === State.Launch
  io.engCfg   := table(idx(log2Ceil(maxLayers) - 1, 0))

  // an empty layer can finish in the same cycle
  def next(): Unit =
    when(idx === num - 1.U) {
      state   := State.Idle
      io.done := true.B
    }.otherwise {
      idx   := idx + 1.U
      state := State.Launch
    }

  switch(state) {
    is(State.Idle) {
      when(io.start) {
        idx := 0.U
        num := Mux(io.numLayers > maxLayers.U, maxLayers.U, io.numLayers)
        when(io.numLayers === 0.U) { io.done := true.B }.otherwise { state := State.Launch }
      }
    }
    is(State.Launch) {
      when(io.engDone) { next() }.otherwise { state := State.Wait }
    }
    is(State.Wait) {
      when(io.engDone) { next() }
    }
  }
}
