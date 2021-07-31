#!/usr/bin/env bash
set -euo pipefail

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

ROOT_DIR=$( dirname "$SCRIPTS_DIR" )
SERVICES_DIR="$ROOT_DIR/services"
HLINT_CONFIG_PATH="$ROOT_DIR/hlint.dhall"

export ROOT_DIR
export SERVICES_DIR
export HLINT_CONFIG_PATH
