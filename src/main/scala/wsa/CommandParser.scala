package wsa

import chisel3._
import chisel3.util._

/** Host protocol over a byte stream. Multi-byte fields are big-endian; addresses and counts are
  * in memory entries of n lanes.
  *
  *   0x01 addr:2 count:2 then count * n bytes       write weights
  *   0x02 addr:2 count:2 then count * n * 4 bytes   write biases (int32)
  *   0x03 addr:2 count:2 then count * n bytes       write activations
  *   0x04 idx:1 wBase:2 bBase:2 inBase:2 outBase:2 m:2 kTiles:1 nTiles:1 shift:1 flags:1
  *                                                  write layer idx (flags bit 0 relu, bit 1 toOut)
  *   0x05 numLayers:1                               run, replies 0xD0 when done
  *   0x06 addr:2 count:2                            read outputs, replies count * n * 4 bytes
  *   0x07                                           ping, replies 0xA5
  *
  * Unknown command bytes are ignored. Incoming bytes wait in a small FIFO while the parser is
  * busy, so the host can send the read command right after a run.
  */
object Cmd {
  val WriteWeights = 0x01
  val WriteBias    = 0x02
  val WriteActs    = 0x03
  val WriteLayer   = 0x04
  val Run          = 0x05
  val ReadOut      = 0x06
  val Ping         = 0x07
  val RunDone      = 0xd0
  val Pong         = 0xa5
}

class CommandParser(n: Int, maxLayers: Int = 16, dataWidth: Int = 8, accWidth: Int = 32, fifoDepth: Int = 32)
    extends Module {
  val io = IO(new Bundle {
    val rx = Flipped(Valid(UInt(8.W)))
    val tx = Decoupled(UInt(8.W))

    val wWrEn       = Output(Bool())
    val bWrEn       = Output(Bool())
    val aWrEn       = Output(Bool())
    val wrAddr      = Output(UInt(16.W))
    val wWrData     = Output(Vec(n, SInt(dataWidth.W)))
    val bWrData     = Output(Vec(n, SInt(accWidth.W)))
    val layerWrEn   = Output(Bool())
    val layerWrIdx  = Output(UInt(log2Ceil(maxLayers).W))
    val layerWrData = Output(new LayerConfig)
    val start       = Output(Bool())
    val numLayers   = Output(UInt(8.W))
    val done        = Input(Bool())
    val outRdAddr   = Output(UInt(16.W))
    val outRdData   = Input(Vec(n, SInt(accWidth.W)))
  })

  object State extends ChiselEnum { val Idle, Header, HeaderDone, Payload, RunWait, ReadAddr, ReadWait, ReadSend, Reply = Value }
  val state = RegInit(State.Idle)

  val cmd    = Reg(UInt(8.W))
  val hdr    = Reg(Vec(15, UInt(8.W)))
  val hdrIdx = Reg(UInt(4.W))
  val hdrLen = Reg(UInt(4.W))
  val addr   = Reg(UInt(16.W))
  val count  = Reg(UInt(16.W))
  val buf    = Reg(Vec(4 * n, UInt(8.W)))
  val bufIdx = Reg(UInt(log2Ceil(4 * n).W))
  val emit   = RegInit(false.B)
  val reply  = Reg(UInt(8.W))

  def u16(i: Int) = Cat(hdr(i), hdr(i + 1))
  val entryBytes  = Mux(cmd === Cmd.WriteBias.U, (4 * n).U, n.U)

  // The UART has no flow control, so bytes queue here while the parser is busy.
  // A byte that arrives with the FIFO full is lost.
  val rxIn = Wire(Decoupled(UInt(8.W)))
  rxIn.valid := io.rx.valid
  rxIn.bits  := io.rx.bits
  val fifo      = Queue(rxIn, fifoDepth)
  val accepting = state === State.Idle || state === State.Header || state === State.Payload
  fifo.ready := accepting
  val byte = fifo.bits
  val got  = fifo.fire

  // Decoded entry from the payload buffer.
  val int8s  = VecInit((0 until n).map(i => buf(i).asSInt))
  val int32s = VecInit((0 until n).map(i => Cat(buf(4 * i), buf(4 * i + 1), buf(4 * i + 2), buf(4 * i + 3)).asSInt))

  io.wWrEn   := emit && cmd === Cmd.WriteWeights.U
  io.bWrEn   := emit && cmd === Cmd.WriteBias.U
  io.aWrEn   := emit && cmd === Cmd.WriteActs.U
  io.wrAddr  := addr
  io.wWrData := int8s
  io.bWrData := int32s
  when(emit) {
    emit := false.B
    addr := addr + 1.U
  }

  val cfg = Wire(new LayerConfig)
  cfg.wBase   := u16(1)
  cfg.bBase   := u16(3)
  cfg.inBase  := u16(5)
  cfg.outBase := u16(7)
  cfg.m       := u16(9)
  cfg.kTiles  := hdr(11)
  cfg.nTiles  := hdr(12)
  cfg.shift   := hdr(13)
  cfg.relu    := hdr(14)(0)
  cfg.toOut   := hdr(14)(1)
  io.layerWrEn   := false.B
  io.layerWrIdx  := hdr(0)
  io.layerWrData := cfg
  io.start       := false.B
  io.numLayers   := hdr(0)
  io.outRdAddr   := addr

  val sendBuf = Reg(Vec(4 * n, UInt(8.W)))
  val sendIdx = Reg(UInt(log2Ceil(4 * n).W))
  io.tx.valid := state === State.Reply || state === State.ReadSend
  io.tx.bits  := Mux(state === State.Reply, reply, sendBuf(sendIdx))

  switch(state) {
    is(State.Idle) {
      when(got) {
        cmd    := byte
        hdrIdx := 0.U
        switch(byte) {
          is(Cmd.WriteWeights.U, Cmd.WriteBias.U, Cmd.WriteActs.U, Cmd.ReadOut.U) {
            hdrLen := 4.U
            state  := State.Header
          }
          is(Cmd.WriteLayer.U) {
            hdrLen := 15.U
            state  := State.Header
          }
          is(Cmd.Run.U) {
            hdrLen := 1.U
            state  := State.Header
          }
          is(Cmd.Ping.U) {
            reply := Cmd.Pong.U
            state := State.Reply
          }
        }
      }
    }
    is(State.Header) {
      when(got) {
        hdr(hdrIdx) := byte
        hdrIdx      := hdrIdx + 1.U
        when(hdrIdx === hdrLen - 1.U) { state := State.HeaderDone }
      }
    }
    is(State.HeaderDone) {
      addr   := u16(0)
      count  := u16(2)
      bufIdx := 0.U
      state  := State.Idle
      switch(cmd) {
        is(Cmd.WriteWeights.U, Cmd.WriteBias.U, Cmd.WriteActs.U) {
          when(u16(2) =/= 0.U) { state := State.Payload }
        }
        is(Cmd.WriteLayer.U) { io.layerWrEn := true.B }
        is(Cmd.Run.U) {
          io.start := true.B
          reply    := Cmd.RunDone.U
          // Zero layers finish in the same cycle.
          state := Mux(io.done, State.Reply, State.RunWait)
        }
        is(Cmd.ReadOut.U) {
          when(u16(2) =/= 0.U) { state := State.ReadAddr }
        }
      }
    }
    is(State.Payload) {
      when(got) {
        buf(bufIdx) := byte
        when(bufIdx === entryBytes - 1.U) {
          bufIdx := 0.U
          emit   := true.B
          count  := count - 1.U
          when(count === 1.U) { state := State.Idle }
        }.otherwise {
          bufIdx := bufIdx + 1.U
        }
      }
    }
    is(State.RunWait) {
      when(io.done) { state := State.Reply }
    }
    is(State.ReadAddr) {
      // outRdAddr is addr; the output memory answers next cycle.
      state := State.ReadWait
    }
    is(State.ReadWait) {
      for (i <- 0 until n; j <- 0 until 4) {
        sendBuf(4 * i + j) := io.outRdData(i).asUInt(31 - 8 * j, 24 - 8 * j)
      }
      sendIdx := 0.U
      state   := State.ReadSend
    }
    is(State.ReadSend) {
      when(io.tx.ready) {
        when(sendIdx === (4 * n - 1).U) {
          addr  := addr + 1.U
          count := count - 1.U
          state := Mux(count === 1.U, State.Idle, State.ReadAddr)
        }.otherwise {
          sendIdx := sendIdx + 1.U
        }
      }
    }
    is(State.Reply) {
      when(io.tx.ready) { state := State.Idle }
    }
  }
}
