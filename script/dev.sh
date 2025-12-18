#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

pnpm i
pnpm dev