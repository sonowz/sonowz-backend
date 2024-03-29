{
  description = "Sonowz core library";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [
        haskellNix.overlay (final: prev: {
          project = final.haskell-nix.stackProject' {
            src = ../.;
            compiler-nix-name = "ghc8107";
          };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.project.flake {};
    in flake // {
      defaultPackage = flake.packages."sonowz-core:lib:sonowz-core";
    });
}