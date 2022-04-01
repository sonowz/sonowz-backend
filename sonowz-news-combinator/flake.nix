{
  description = "Check news and create notification";

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
      app = flake.packages."sonowz-news-combinator:exe:sonowz-news-combinator";
    in flake // {
      defaultPackage = app;

      nixosModules.sonowz-news-combinator =
        { config, lib, pkgs, ... }:
        let cfg = config.services.sonowz-news-combinator;
        in
        {
          options.services.sonowz-news-combinator = {
            enable = lib.mkEnableOption "sonowz-news-combinator";
            options = lib.mkOption {
              description = "Command line arguments. '--help' to print options.";
              type = lib.types.str;
            };
          };

          config = lib.mkIf cfg.enable {
            systemd.services.sonowz-news-combinator = {
              enable = true;
              path = [ app ];
              script = "sonowz-news-combinator ${cfg.options}";
            };
          };
        };
    });
}