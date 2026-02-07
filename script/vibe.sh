#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

vibe --send "claude --allow-dangerously-skip-permissions --dangerously-skip-permissions"