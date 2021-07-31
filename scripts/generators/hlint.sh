#!/usr/bin/env bash
set -euo pipefail

dhall-to-yaml-ng --generated-comment --file "$HLINT_CONFIG_PATH" --output "$ROOT_DIR/.hlint.yaml"
