{
  description = "Cascade";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/20dc478985d6545df53f0153f4af125eb014083d";
    # nixpkgs-unstable.url = "github:NixOS/nixpkgs/f677051b8dc0b5e2a9348941c99eea8c4b0ff28f";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/41ac0bd371618db6dd67fd952cc5b3d6a9955a15";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs-stable, nixpkgs-unstable, utils, ... }:
    let name = "cascade";
    in
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs-stable {
          inherit system;
          overlays = [
            (final: _:
              {
                project = rec {
                  compilerVersion = "924";
                  haskellPackages = final.unstable.haskell.packages.${"ghc${compilerVersion}"};
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
