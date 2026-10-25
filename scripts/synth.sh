#!/bin/sh
# Generates the Verilog and prints yosys resource counts for ECP5 and iCE40.
# Usage: scripts/synth.sh [n] (array size, default 4)
set -e
cd "$(dirname "$0")/.."
sbt -batch "runMain wsa.Generate ${1:-4}"
files=$(sed 's|^|generated/|' generated/filelist.f | tr '\n' ' ')
for target in ecp5 ice40; do
  echo "== $target"
  yosys -q -p "read_verilog -sv $files; synth_$target -top Top; tee -q -o /dev/stdout stat" | grep -E "^ +[0-9]+ +(cells|[A-Z][A-Z_0-9]+)$"
done
