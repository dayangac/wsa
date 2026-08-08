package wsa

import chisel3._

/** The accelerator without its host link: the memories, the engine and the layer sequencer,
  * with ports to fill the memories and the layer table, run the network and read results back.
  *
  * Host reads of the activation and output memories are synchronous and only valid while the
  * engine is not busy, since the engine uses the same read ports.
  */
class Core(
  val n:           Int = 4,
  val weightDepth: Int = 4096,
  val actDepth:    Int = 2048,
  val biasDepth:   Int = 256,
  val outDepth:    Int = 1024,
  val maxRows:     Int = 256,
  val maxLayers:   Int = 16,
  dataWidth:       Int = 8,
  accWidth:        Int = 32)
    extends Module {
  val io = IO(new Bundle {
    val wWrEn   = Input(Bool())
    val wWrAddr = Input(UInt(16.W))
    val wWrData = Input(Vec(n, SInt(dataWidth.W)))
    val bWrEn   = Input(Bool())
    val bWrAddr = Input(UInt(16.W))
    val bWrData = Input(Vec(n, SInt(accWidth.W)))
    val aWrEn   = Input(Bool())
    val aWrAddr = Input(UInt(16.W))
    val aWrData = Input(Vec(n, SInt(dataWidth.W)))

    val aRdAddr   = Input(UInt(16.W))
    val aRdData   = Output(Vec(n, SInt(dataWidth.W)))
    val outRdAddr = Input(UInt(16.W))
    val outRdData = Output(Vec(n, SInt(accWidth.W)))

    val layerWrEn   = Input(Bool())
    val layerWrIdx  = Input(UInt(chisel3.util.log2Ceil(maxLayers).W))
    val layerWrData = Input(new LayerConfig)

    val start     = Input(Bool())
    val numLayers = Input(UInt(8.W))
    val busy      = Output(Bool())
    val done      = Output(Bool())
  })

  val engine = Module(new MatMulEngine(n, maxRows, dataWidth, accWidth))
  val seq    = Module(new LayerSequencer(maxLayers))
  seq.io.tableWrEn   := io.layerWrEn
  seq.io.tableWrIdx  := io.layerWrIdx
  seq.io.tableWrData := io.layerWrData
  seq.io.start       := io.start
  seq.io.numLayers   := io.numLayers
  engine.io.start    := seq.io.engStart
  engine.io.cfg      := seq.io.engCfg
  seq.io.engDone     := engine.io.done
  io.busy            := seq.io.busy
  io.done            := seq.io.done

  val weightMem = SyncReadMem(weightDepth, Vec(n, SInt(dataWidth.W)))
  val biasMem   = SyncReadMem(biasDepth, Vec(n, SInt(accWidth.W)))
  val actMem    = SyncReadMem(actDepth, Vec(n, SInt(dataWidth.W)))
  val outMem    = SyncReadMem(outDepth, Vec(n, SInt(accWidth.W)))

  when(io.wWrEn) { weightMem.write(io.wWrAddr, io.wWrData) }
  engine.io.wData := weightMem.read(engine.io.wAddr)

  when(io.bWrEn) { biasMem.write(io.bWrAddr, io.bWrData) }
  engine.io.bData := biasMem.read(engine.io.bAddr)

  when(engine.io.actWrEn) {
    actMem.write(engine.io.actWrAddr, engine.io.actWrData)
  }.elsewhen(io.aWrEn) {
    actMem.write(io.aWrAddr, io.aWrData)
  }
  val actRd = actMem.read(Mux(engine.io.busy, engine.io.aAddr, io.aRdAddr))
  engine.io.aData := actRd
  io.aRdData      := actRd

  when(engine.io.outWrEn) { outMem.write(engine.io.outWrAddr, engine.io.outWrData) }
  io.outRdData := outMem.read(io.outRdAddr)
}
