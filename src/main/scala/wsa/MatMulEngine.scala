package wsa

import chisel3._
import chisel3.util._

/** One layer: out = post(act x W + bias), with act M x K and W K x N, K and N multiples of n. */
class LayerConfig extends Bundle {
  val wBase   = UInt(16.W) // first weight tile
  val bBase   = UInt(16.W) // bias entry of output tile 0
  val inBase  = UInt(16.W) // input activations, in the activation memory
  val outBase = UInt(16.W) // results, in the activation memory or the output memory
  val m       = UInt(16.W) // rows (inputs in the batch)
  val kTiles  = UInt(8.W)  // K / n
  val nTiles  = UInt(8.W)  // N / n
  val shift   = UInt(5.W)
  val relu    = Bool()
  val toOut   = Bool() // write int32 results to the output memory instead of int8 to activations
}

/** Runs one layer on a weight-stationary array.
  *
  * Memory layout, every entry being n lanes:
  *   - weights: tile (kt, nt) is n entries at wBase + (nt * kTiles + kt) * n, entry r holding
  *     W(kt * n + r)(nt * n until (nt + 1) * n)
  *   - input: row m of K-tile kt at inBase + kt * M + m
  *   - bias: output tile nt at bBase + nt
  *   - result: row m of output tile nt at outBase + nt * M + m, which is also the input layout of
  *     the next layer
  *
  * Tiles run with nt outer and kt inner. The loader fills the shadow weights of the next tile
  * as soon as the swap of the current one has reached the last PE, so loads hide behind
  * streams. Partial sums for the first K - 1 tiles go to the accumulator memory; the last K tile
  * adds the accumulator, runs post processing and writes the result.
  *
  * All memories are synchronous: data comes back the cycle after the address.
  */
class MatMulEngine(n: Int, maxRows: Int, dataWidth: Int = 8, accWidth: Int = 32) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val cfg   = Input(new LayerConfig)
    val busy  = Output(Bool())
    val done  = Output(Bool())

    val wAddr = Output(UInt(16.W))
    val wData = Input(Vec(n, SInt(dataWidth.W)))
    val aAddr = Output(UInt(16.W))
    val aData = Input(Vec(n, SInt(dataWidth.W)))
    val bAddr = Output(UInt(16.W))
    val bData = Input(Vec(n, SInt(accWidth.W)))

    val actWrEn   = Output(Bool())
    val actWrAddr = Output(UInt(16.W))
    val actWrData = Output(Vec(n, SInt(dataWidth.W)))
    val outWrEn   = Output(Bool())
    val outWrAddr = Output(UInt(16.W))
    val outWrData = Output(Vec(n, SInt(accWidth.W)))
  })

  val array  = Module(new SystolicArray(n, dataWidth, accWidth))
  val post   = Module(new PostProcess(n, accWidth, dataWidth))
  val accMem = SyncReadMem(maxRows, Vec(n, SInt(accWidth.W)))

  val cfg        = Reg(new LayerConfig)
  val running    = RegInit(false.B)
  val totalTiles = cfg.kTiles * cfg.nTiles

  // Loader state
  val loading    = RegInit(false.B)
  val loadRow    = Reg(UInt(log2Ceil(n + 1).W))
  val loaded     = Reg(UInt(16.W))
  val tileBase   = Reg(UInt(16.W))
  val shadowFull = RegInit(false.B)
  val sinceSwap  = Reg(UInt(log2Ceil(2 * n + 1).W))
  val swapSafe   = sinceSwap >= array.swapDistance.U

  // Streamer state
  val streaming   = RegInit(false.B)
  val row         = Reg(UInt(16.W))
  val streamed    = Reg(UInt(16.W))
  val kt          = Reg(UInt(8.W))
  val nt          = Reg(UInt(8.W))
  val actTileBase = Reg(UInt(16.W))
  val dstTileBase = Reg(UInt(16.W))

  val draining = RegInit(false.B)
  val drain    = Reg(UInt(log2Ceil(2 * n + 4).W))

  io.done   := false.B
  io.busy   := running
  io.wAddr  := 0.U
  io.aAddr  := 0.U

  when(io.start && !running) {
    cfg         := io.cfg
    loaded      := 0.U
    streamed    := 0.U
    tileBase    := io.cfg.wBase
    actTileBase := io.cfg.inBase
    dstTileBase := io.cfg.outBase
    kt          := 0.U
    nt          := 0.U
    shadowFull  := false.B
    sinceSwap   := array.swapDistance.U
    // An empty layer has nothing to do.
    when(io.cfg.m === 0.U || io.cfg.kTiles === 0.U || io.cfg.nTiles === 0.U) {
      io.done := true.B
    }.otherwise {
      running := true.B
    }
  }

  when(sinceSwap < array.swapDistance.U) { sinceSwap := sinceSwap + 1.U }

  // Loader: one weight row per cycle, last row first.
  val loadIssue = WireDefault(false.B)
  when(running && !loading && !shadowFull && swapSafe && loaded < totalTiles) {
    loading := true.B
    loadRow := 0.U
  }
  when(loading) {
    loadIssue := true.B
    io.wAddr  := tileBase + (n - 1).U - loadRow
    when(loadRow === (n - 1).U) {
      loading    := false.B
      loaded     := loaded + 1.U
      tileBase   := tileBase + n.U
      shadowFull := true.B
    }.otherwise {
      loadRow := loadRow + 1.U
    }
  }
  array.io.shift    := RegNext(loadIssue, false.B)
  array.io.weightIn := io.wData

  // Streamer: one input row per cycle; the first row of a tile carries the swap.
  class Tag extends Bundle {
    val row   = UInt(16.W)
    val first = Bool()
    val last  = Bool()
    val bAddr = UInt(16.W)
    val dst   = UInt(16.W)
  }
  val issueValid = WireDefault(false.B)
  val issueSwap  = WireDefault(false.B)
  val issueTag   = Wire(new Tag)
  issueTag.row   := row
  issueTag.first := kt === 0.U
  issueTag.last  := kt === cfg.kTiles - 1.U
  issueTag.bAddr := cfg.bBase + nt
  issueTag.dst   := dstTileBase + row

  when(running && !streaming && shadowFull && streamed < totalTiles) {
    streaming := true.B
    row       := 0.U
  }
  when(streaming) {
    issueValid := true.B
    io.aAddr   := actTileBase + row
    when(row === 0.U) {
      issueSwap  := true.B
      shadowFull := false.B
      sinceSwap  := 0.U
    }
    when(row === cfg.m - 1.U) {
      streaming := false.B
      streamed  := streamed + 1.U
      when(kt === cfg.kTiles - 1.U) {
        kt          := 0.U
        nt          := nt + 1.U
        actTileBase := cfg.inBase
        dstTileBase := dstTileBase + cfg.m
      }.otherwise {
        kt          := kt + 1.U
        actTileBase := actTileBase + cfg.m
      }
    }.otherwise {
      row := row + 1.U
    }
  }

  // The activation memory answers a cycle later; the tag follows the data through the array.
  array.io.inValid := RegNext(issueValid, false.B)
  array.io.swap    := RegNext(issueSwap, false.B)
  array.io.actIn   := io.aData
  val tagIn = RegNext(issueTag)

  // One cycle before a result leaves the array, read its accumulator and bias entries.
  val readValid = ShiftRegister(array.io.inValid, array.latency - 1, false.B, true.B)
  val readTag   = ShiftRegister(tagIn, array.latency - 1)
  val accOld    = accMem.read(readTag.row, readValid && !readTag.first)
  io.bAddr := readTag.bAddr

  val outTag = RegNext(readTag)
  val sum    = VecInit(array.io.out.zip(accOld).map { case (y, a) => Mux(outTag.first, y, y + a) })

  post.io.in    := sum
  post.io.bias  := io.bData
  post.io.shift := cfg.shift
  post.io.relu  := cfg.relu

  val result = array.io.outValid && outTag.last
  when(array.io.outValid && !outTag.last) { accMem.write(outTag.row, sum) }
  io.actWrEn   := result && !cfg.toOut
  io.actWrAddr := outTag.dst
  io.actWrData := post.io.outClamped
  io.outWrEn   := result && cfg.toOut
  io.outWrAddr := outTag.dst
  io.outWrData := post.io.out

  // Done once every tile is streamed and the last result has left the pipeline.
  when(running && !draining && !streaming && streamed === totalTiles) {
    draining := true.B
    drain    := (array.latency + 2).U
  }
  when(draining) {
    drain := drain - 1.U
    when(drain === 0.U) {
      draining := false.B
      running  := false.B
      io.done  := true.B
    }
  }
}
