#!/usr/bin/env bash
set -euo pipefail

while sleep 1; do
    git ls-files -cmo | egrep ".hs|.dhall" | entr -cdr ./scripts/cabal.sh
done
