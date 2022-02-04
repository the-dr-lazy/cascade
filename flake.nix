{
  description = "Cascade";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs-stable, nixpkgs-unstable, utils, ... }:
    let name = "cascade";
      # See https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-976899227
    in
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        workaround140774 = package:
          pkgs.haskell.lib.overrideCabal package (_: {
            enableSeparateBinOutput = false;
          });
        pkgs = import nixpkgs-stable {
          inherit system;
          overlays = [
            (final: _:
              {
                project = rec {
                  compilerVersion = "8107";
                  haskellPackages = final.haskell.packages.${"ghc${compilerVersion}"}.override {
                    overrides = _: super: {
                      ghcid = workaround140774 super.ghcid;
                      ormolu = workaround140774 super.ormolu;
                    };
                  };
                };
                unstable = import nixpkgs-unstable { inherit system; };
              })
          ];
        };
      in
      {
        devShell = import ./shell.nix { inherit name pkgs; };
      });
}
