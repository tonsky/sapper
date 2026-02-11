#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

./script/build.sh
pnpm i
pnpm dev
