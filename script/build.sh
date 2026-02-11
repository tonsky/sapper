#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

zig build-exe src-zig/solver.zig -target wasm32-freestanding -fno-entry -O ReleaseFast -femit-bin=public/solver.wasm --export=solve --export=hint --export=getInputBuf --export=getOutputBuf
echo "[ OK ] Build public/solver.wasm"
ls -lah public/solver.wasm