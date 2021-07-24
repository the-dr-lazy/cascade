#! /bin/bash

for file in $(find . -type f -path "./*/*" -name *.dhall); do
	dhall-hpack-cabal --package-dhall $file
done
