# wsa

A small neural network accelerator in Chisel, built around a weight-stationary systolic array.
A host sends a quantized MLP and a batch of inputs over a UART, the accelerator runs every
layer, and the host reads the results back. The test suite runs a digits classifier end to end
in simulation.

## Architecture

### Processing element

Each PE holds one int8 weight. Every cycle it multiplies the activation coming from the left
by that weight, adds the partial sum coming from above, and passes the result down. The
activation moves on to the PE on the right.

The weight is double buffered. A second "shadow" register is loaded while the PE keeps
computing with the active one. A swap flag travels along with the activations: the activation
that carries it is the first one to use the new weight.

### Systolic array

n x n PEs compute y = x W for one input vector per cycle. PE (r, c) holds W[r][c]. Input row r
is delayed by r cycles so it meets the partial sums coming down, and the outputs are delayed
again so the whole result vector comes out together, 2n - 1 cycles after its input went in.

Weights are loaded down each column as a shift chain, one row per cycle, into the shadow
registers. The next tile can start loading 2n - 2 cycles after the previous swap entered the
array, so loads hide behind streaming.

### Engine

The engine runs one layer, out = post(act x W + bias), with act M x K and W K x N, by splitting
it into n x n tiles. Output tiles run in the outer loop and K tiles in the inner loop. For each
tile the engine:

1. loads the tile's weights into the shadow registers while the previous tile streams
2. streams all M input rows through the array, with the swap on the first row
3. adds each result row to an accumulator memory, or for the last K tile, adds the bias,
   applies a rounding right shift and an optional ReLU, and writes the result

A hidden layer's results are clamped to int8 and written back into the activation memory, in
the same layout the next layer reads. The last layer writes int32 results to an output memory.

M is the batch size, so a whole batch of inputs shares each weight tile. That reuse is what a
weight-stationary array is good at.

### Layer sequencer

A table holds up to 16 layer configurations (memory bases, M, tile counts, shift, ReLU, output
target). One start command runs the layers in order, ping-ponging between two regions of the
activation memory.

### Host link

A UART receiver and transmitter (8N1) connect to a command parser. Incoming bytes wait in a
small FIFO while the parser is busy. Commands:

    0x01 addr count data   write weights
    0x02 addr count data   write biases (int32, big-endian)
    0x03 addr count data   write activations
    0x04 idx config        write a layer configuration
    0x05 layers            run, replies 0xD0 when done
    0x06 addr count        read results
    0x07                   ping, replies 0xA5

## Layout

    src/main/scala/wsa/   hardware: PE, SystolicArray, PostProcess, MatMulEngine,
                          LayerSequencer, Core, Uart, CommandParser, Accelerator, Top
    src/test/scala/wsa/   ChiselSim tests for every module, and an end to end test over UART
    host/                 python: bit-exact model of the hardware, protocol encoder,
                          numpy trainer for the digits MLP
    scripts/synth.sh      Verilog generation and yosys resource counts

## Running

Tests (needs Verilator):

    sbt test

On an Apple Silicon Mac, sbt and the perl that Verilator starts both have to be arm64:

    export JAVA_HOME=$(/usr/libexec/java_home -a arm64 -v 21)
    PATH="/usr/bin:$PATH" sbt test

Host side (needs numpy and scikit-learn):

    cd host
    python3 -m unittest test_host
    python3 train.py        # retrains and rewrites the test program in src/test/resources/digits

Verilog and resource counts (needs yosys):

    scripts/synth.sh

## Results

- The digits MLP (64-32-10, int8) gets 96.7% on the 450 held-out scikit-learn digits, the same
  as the float model it was quantized from.
- In simulation, 32 test images go through the whole path (UART, parser, two layers on a 4 x 4
  array, UART back). The logits match the python model exactly and all 32 are classified
  right.
- With n = 4, yosys maps the design to about 5200 LUTs, 3700 flip-flops, 28 block RAMs and 17
  multipliers on ECP5. It does not fit an iCE40 UP5K with the default memory sizes.

It has only been run in simulation, not on an FPGA.
