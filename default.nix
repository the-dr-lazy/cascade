{ pkgs ? import ./nix { } }:

pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "cascade";
    src = ./.;
  };
  compiler-nix-name = "ghc884";
}
