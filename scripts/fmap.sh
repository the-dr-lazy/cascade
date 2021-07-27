#!/usr/bin/env bash
set -euo pipefail

for argument in "${@:2}"; do
    eval "$1 $argument"
done
