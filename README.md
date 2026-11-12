# wsa

A small neural network accelerator in Chisel, built around a weight-stationary systolic array.
A host sends a quantized MLP and its inputs over a UART, the hardware runs every layer, and the
host reads the results back.

## Architecture

- **PE:** holds one int8 weight, multiplies the activation coming from the left and adds the
  partial sum coming from above. Weights are double buffered, so the next tile loads while the
  current one computes.
- **Systolic array:** n x n PEs. Activations move right, partial sums move down, one result
  vector comes out every cycle.
- **Engine:** splits a layer into n x n tiles, accumulates across tiles, then applies bias,
  shift, ReLU and an int8 clamp.
- **Layer sequencer:** runs up to 16 layers back to back.
- **Host link:** UART and a byte command parser.

Generated SystemVerilog is in `generated/`.

## Running

    sbt test                      # hardware tests (needs Verilator)
    sbt "runMain wsa.Generate"    # regenerate generated/
    cd host && python3 train.py   # train the digits MLP and write the test program

A 64-32-10 digits classifier runs end to end in simulation. It has not been run on an FPGA.
