#!/usr/bin/env bash
set -euo pipefail

while sleep 1; do
    git ls-files -cmo | grep -E ".hs|.dhall" | entr -cdr ./scripts/cabal.sh
done
