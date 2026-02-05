#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

node --cpu-prof --cpu-prof-name=out/bench.cpuprofile dev/bench.mjs
speedscope out/bench.cpuprofile
