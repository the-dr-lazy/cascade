#!/usr/bin/env bash
set -euo pipefail

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
# shellcheck source=scripts/variables.sh
source "$SCRIPTS_DIR/variables.sh"

function watch_cabal_files() {
  while sleep 1; do
      git ls-files -cmo --no-empty-directory --deduplicate --exclude-standard \
    | grep -E ".hs|.dhall" \
    | entr -npdr "$SCRIPTS_DIR/cabal.sh"
  done
}

function watch_hlint_config() {
    echo "$HLINT_CONFIG_PATH" \
  | entr -pnr dhall-to-yaml-ng --generated-comment --file "$HLINT_CONFIG_PATH" --output "$ROOT_DIR/.hlint.yaml"
}

(trap 'kill 0' SIGINT; watch_cabal_files & watch_hlint_config)
