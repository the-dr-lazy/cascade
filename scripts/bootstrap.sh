#!/usr/bin/env bash
set -euo pipefail

for file in $(git ls-files -cmo | grep -E "cascade-\w+/package.dhall\$"); do
	dhall-hpack-cabal --package-dhall "$file"
done

[[ "${CI:-false}" == "true" ]] || pre-commit install
