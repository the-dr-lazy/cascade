{ pkgs ? import ./nix { }, compiler ? "ghc884" }:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = final: prev: { haskeline = final.haskeline_0_8_1_2; };
  };

  ci = builtins.getEnv "CI" == "true";

  scriptsDir = builtins.toString ./scripts;
in
haskellPackages.shellFor {
  name = "Cascade";
  packages = _: [ ];
  buildInputs = builtins.concatMap builtins.attrValues [
    ###################################################
    # Command line tools:
    {
      inherit (pkgs) headroom hlint nixpkgs-fmt shellcheck stylish-haskell;
      inherit (pkgs.nodePackages) prettier;

      stan = pkgs.haskell.lib.justStaticExecutables haskellPackages.stan;
    }

    ###################################################
    # Command line tools:
    {
      inherit (pkgs) entr ghcid gitFull sqitchPg;
      hpack-dhall = pkgs.haskell.lib.justStaticExecutables haskellPackages.hpack-dhall;
    }

    ###################################################
    # Languages:
    { inherit (pkgs) dhall; }

    ###################################################
    # Nativ libraries:
    {
      inherit (pkgs) libjwt postgresql_13;
      zlib = pkgs.zlib.dev;
    }

    ###################################################
    # LSPs:
    {
      inherit (pkgs) dhall-lsp-server haskell-language-server;
      inherit (pkgs.nodePackages) bash-language-server yaml-language-server;
    }

    ###################################################
    # Package managers:
    {
      inherit (pkgs) cabal-install niv;
    }
  ];

  shellHook = ''
    ${scriptsDir}/cabal.sh
  '';
}
