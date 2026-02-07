#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

./script/zig_build.sh
./out/solver bench "$@"
