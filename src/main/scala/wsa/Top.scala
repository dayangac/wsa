package wsa

import chisel3._
import _root_.circt.stage.ChiselStage

/** The accelerator on a serial line: 8N1 UART in and out at clock / clocksPerBit baud. */
class Top(n: Int = 4, clocksPerBit: Int = 868) extends Module {
  val io = IO(new Bundle {
    val rxd  = Input(Bool())
    val txd  = Output(Bool())
    val busy = Output(Bool())
  })

  val rx  = Module(new UartRx(clocksPerBit))
  val tx  = Module(new UartTx(clocksPerBit))
  val acc = Module(new Accelerator(n))

  rx.io.rxd  := io.rxd
  acc.io.rx  := rx.io.out
  tx.io.in <> acc.io.tx
  io.txd     := tx.io.txd
  io.busy    := acc.io.busy
}

/** Writes generated/Top.sv. Arguments: n, clock in Hz, baud (default 4, 100 MHz, 115200). */
object Generate extends App {
  val n     = args.lift(0).map(_.toInt).getOrElse(4)
  val clock = args.lift(1).map(_.toInt).getOrElse(100000000)
  val baud  = args.lift(2).map(_.toInt).getOrElse(115200)
  ChiselStage.emitSystemVerilogFile(
    new Top(n, clock / baud),
    args = Array("--target-dir", "generated"),
    // Plain Verilog-2005 style output, so yosys can read it.
    firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-lowering-options=disallowLocalVariables,disallowPackedArrays"
    )
  )
}
