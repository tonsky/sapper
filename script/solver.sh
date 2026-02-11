#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

if [ "${1:-}" = "test" ]; then
  zig test src-zig/cli.zig
else
  OPT="-O ReleaseFast"
  BUILD=true
  ARGS=()
  for arg in "$@"; do
    if [ "$arg" = "--debug" ]; then
      OPT=""
    elif [ "$arg" = "--no-build" ]; then
      BUILD=false
    else
      ARGS+=("$arg")
    fi
  done
  if [ $BUILD = true ]; then
      zig build-exe src-zig/cli.zig $OPT -femit-bin=out/solver
  fi
  ./out/solver "${ARGS[@]}"
fi
