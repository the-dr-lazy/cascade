#!/usr/bin/env bash
set -euo pipefail

for file in $(git ls-files -cmo | egrep "[^\/]+/package.dhall"); do
	dhall-hpack-cabal --package-dhall $file
done
