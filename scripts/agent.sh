#!/usr/bin/env bash

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

set -euo pipefail

while sleep 1; do
    git ls-files -cmo --no-empty-directory --deduplicate --exclude-standard | grep -E ".hs|.dhall" | entr -ndr "$SCRIPTS_DIR/cabal.sh"
done
