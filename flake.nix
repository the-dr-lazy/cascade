{
  description = "Cascade";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: _: { project.haskellPackages = final.haskell.packages.ghc884; })
          ];
        };
      in
      {
        devShell = import ./shell.nix { inherit pkgs; };
      });
}
