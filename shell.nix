{ name, pkgs }:

pkgs.project.haskellPackages.shellFor {
  inherit name;

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
      inherit (pkgs)
        pre-commit
        hlint
        nixpkgs-fmt
        nix-linter
        shellcheck
        shfmt
        stylish-haskell;
      inherit (pkgs.unstable.python310Packages) pre-commit-hooks yamllint;
      inherit (pkgs.nodePackages) prettier;


      # stan = pkgs.haskell.lib.justStaticExecutables pkgs.project.haskellPackages.stan;
      headroom = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.headroom;
    }

    ###################################################
    # Command line tools:
    {
      inherit (pkgs)
        entr
        git
        git-lfs
        sqitchPg;

      # ghcid = pkgs.haskell.lib.justStaticExecutables pkgs.project.haskellPackages.ghcid;
      hpack-dhall = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hpack-dhall;
      dhall-yaml = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.dhall-yaml;
    }

    ###################################################
    # Languages:
    { inherit (pkgs) dhall; }


    ###################################################
    # Language servers:
    {
      inherit (pkgs)
        dhall-lsp-server;
      inherit (pkgs.nodePackages)
        bash-language-server
        yaml-language-server
        vscode-json-languageserver-bin;

      haskell-language-server = pkgs.haskell.lib.justStaticExecutables pkgs.project.haskellPackages.haskell-language-server;
    }

    ###################################################
    # Package managers:
    { inherit (pkgs) cabal-install; }
  ];
}
