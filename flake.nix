{
  description = "Cascade";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/ca087b7e4f67fd9d08313d241ba398201a604e9c";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs-unstable, utils, ... }:
    let name = "cascade";
    in
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs-unstable {
          inherit system;
          overlays = [
            (final: _:
              {
                project = rec {
                  compilerVersion = "922";
                  haskellPackages = final.haskell.packages.${"ghc${compilerVersion}"};
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
