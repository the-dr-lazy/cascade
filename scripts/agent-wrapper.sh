#!/usr/bin/env bash

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

nix --no-warn-dirty develop -c "$SCRIPTS_DIR/agent.sh"
