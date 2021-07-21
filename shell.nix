{ pkgs ? import ./nix { }, project ? import ./default.nix { } }:

# let
#   haskellPackages = pkgs.haskell.packages.${compiler}.override {
#     overrides = final: prev: { haskeline = final.haskeline_0_8_1_2; };
#   };

project.shellFor {
  packages = ps: with ps; [ cascade-cli ];

  withHoogle = true;

  buildInputs = with pkgs; [
    ###################################################
    # Code styles:
    (pkgs.haskell.lib.justStaticExecutables haskellPackages.stan)
    headroom
    hlint
    nixpkgs-fmt
    nodePackages.prettier
    shellcheck
    stylish-haskell

    ###################################################
    # Command line tools:
    (pkgs.haskell.lib.justStaticExecutables haskellPackages.hpack-dhall)
    entr
    ghcid
    gitFull
    sqitchPg

    ###################################################
    # Languages:
    dhall

    ###################################################
    # Libraries:
    libjwt
    postgresql_13
    zlib.dev

    ###################################################
    # LSPs:
    dhall-lsp-server
    haskell-language-server
    nodePackages.bash-language-server
    nodePackages.yaml-language-server

    ###################################################
    # Package managers:
    cabal-install
    niv
  ];

  exactDeps = true;
}
