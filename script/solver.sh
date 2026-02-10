#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

if [ "${1:-}" = "test" ]; then
  zig test src/sapper/solver.zig
else
  OPT="-O ReleaseFast"
  ARGS=()
  for arg in "$@"; do
    if [ "$arg" = "--debug" ]; then
      OPT=""
    else
      ARGS+=("$arg")
    fi
  done
  zig build-exe src/sapper/solver.zig $OPT -femit-bin=out/solver
  ./out/solver "${ARGS[@]}"
fi
