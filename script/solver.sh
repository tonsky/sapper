#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

if [ "${1:-}" = "test" ]; then
  zig test src/sapper/solver.zig
else
  zig build-exe src/sapper/solver.zig -O ReleaseFast -femit-bin=out/solver
  ./out/solver "$@"
fi
