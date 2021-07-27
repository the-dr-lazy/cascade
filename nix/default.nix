{ system ? builtins.currentSystem, compiler ? "ghc884", sources ? import ./sources.nix { } }:

import sources.nixpkgs {
  inherit system;
  config = { };
  overlays = import ./overlays.nix { inherit system compiler sources; };
}
