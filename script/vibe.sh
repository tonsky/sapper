#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

vibe --send "IS_SANDBOX=1 claude --allow-dangerously-skip-permissions --dangerously-skip-permissions"