#!/usr/bin/env bash
set -euo pipefail

for file in $(git ls-files -cmo | grep -E "[^\/]+/package.dhall"); do
	dhall-hpack-cabal --package-dhall "$file"
done

pre-commit install
