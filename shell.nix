{ pkgs }:

pkgs.project.haskellPackages.shellFor {
  name = "Cascade";

  packages = _: [ ];

  buildInputs = builtins.concatMap builtins.attrValues [
    ###################################################
    # Native libraries:
    {
      inherit (pkgs) libjwt postgresql_13;

      zlib = pkgs.zlib.dev;
    }
  ];

  nativeBuildInputs = builtins.concatMap builtins.attrValues [
    ###################################################
    # Code styles:
    {
      inherit (pkgs) pre-commit hlint nixpkgs-fmt nix-linter shellcheck shfmt stylish-haskell;
      inherit (pkgs.python3Packages) pre-commit-hooks yamllint;
      inherit (pkgs.nodePackages) prettier;

      stan = pkgs.haskell.lib.justStaticExecutables pkgs.project.haskellPackages.stan;
      headroom = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.headroom;
    }

    ###################################################
    # Command line tools:
    {
      inherit (pkgs) entr ghcid git git-lfs sqitchPg;

      hpack-dhall = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hpack-dhall;
      dhall-yaml = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.dhall-yaml;
    }

    ###################################################
    # Languages:
    { inherit (pkgs) dhall; }


    ###################################################
    # Language servers:
    {
      inherit (pkgs) dhall-lsp-server haskell-language-server;
      inherit (pkgs.nodePackages) bash-language-server yaml-language-server vscode-json-languageserver-bin;
    }

    ###################################################
    # Package managers:
    { inherit (pkgs) cabal-install; }
  ];
}
