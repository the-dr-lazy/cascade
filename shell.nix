{ pkgs ? import ./nix { }, compiler ? "ghc884" }:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = final: prev: { haskeline = final.haskeline_0_8_1_2; };
  };

in haskellPackages.shellFor {
  packages = _: [ ];
  buildInputs = with pkgs; [
    # Libraries:
    zlib.dev
    libjwt
    postgresql_13

    # Dev tools:
    gitFull
    niv
    headroom
    cabal-install
    haskell-language-server
    dhall
    dhall-lsp-server
    (pkgs.haskell.lib.justStaticExecutables haskellPackages.hpack-dhall)
    (pkgs.haskell.lib.justStaticExecutables haskellPackages.stan)
    ghcid
    hlint
    stylish-haskell
    nixpkgs-fmt
    sqitchPg
    nodePackages.prettier
    nodePackages.yaml-language-server
    nodePackages.bash-language-server
    shellcheck
  ];
  shellHook = ''
    make setup
  '';
}
