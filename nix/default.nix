{ system ? builtins.currentSystem, sources ? import ./sources.nix { } }:

let
  headroom = if system == "x86_64-darwin" then
    sources.headroom-darwin
  else if system == "x86_64-linux" then
    sources.headroom-linux
  else
    null;

  haskell-nix = import sources.haskell-nix { };

  overlays = haskell-nix.nixpkgsArgs.overlays ++ [
    (final: _: {
      headroom = if headroom != null then
        final.runCommand "headroom" { } ''
          mkdir -p $out/bin
          cp ${headroom} $out/bin/headroom
          chmod +x $out/bin/headroom
        ''
      else
        throw "There is no Headroom executable for this system architecture.";
    })
  ];

in import haskell-nix.sources.nixpkgs-2105
(haskell-nix.nixpkgsArgs // { inherit overlays; })
