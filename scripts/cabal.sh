#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=./nix -i bash -p hpack-dhall --pure

set -euo pipefail

for file in $(find . -type f -path "./*/*" -name *.dhall); do
	dhall-hpack-cabal --package-dhall $file
done
