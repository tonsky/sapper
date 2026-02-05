#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

zig build-exe src/sapper/solver.zig -O ReleaseFast -femit-bin=out/solver
./out/solver
