package wsa

import chisel3._
import chisel3.util._

/** The accelerator behind a byte stream: the command parser driving the core. */
class Accelerator(n: Int = 4) extends Module {
  val io = IO(new Bundle {
    val rx = Flipped(Valid(UInt(8.W)))
    val tx   = Decoupled(UInt(8.W))
    val busy = Output(Bool())
  })
  val parser = Module(new CommandParser(n))
  val core   = Module(new Core(n = n))
  parser.io.rx <> io.rx
  io.tx <> parser.io.tx
  core.io.wWrEn       := parser.io.wWrEn
  core.io.wWrAddr     := parser.io.wrAddr
  core.io.wWrData     := parser.io.wWrData
  core.io.bWrEn       := parser.io.bWrEn
  core.io.bWrAddr     := parser.io.wrAddr
  core.io.bWrData     := parser.io.bWrData
  core.io.aWrEn       := parser.io.aWrEn
  core.io.aWrAddr     := parser.io.wrAddr
  core.io.aWrData     := parser.io.wWrData
  core.io.aRdAddr     := 0.U
  core.io.layerWrEn   := parser.io.layerWrEn
  core.io.layerWrIdx  := parser.io.layerWrIdx
  core.io.layerWrData := parser.io.layerWrData
  core.io.start       := parser.io.start
  core.io.numLayers   := parser.io.numLayers
  parser.io.done      := core.io.done
  core.io.outRdAddr   := parser.io.outRdAddr
  parser.io.outRdData := core.io.outRdData
  io.busy             := core.io.busy
}
