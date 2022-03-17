{
  description = "Raytrace demo generator";

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
            compiler-nix-name = "ghc884";
          };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.project.flake {};
      app = flake.packages."sonowz-raytrace:exe:sonowz-raytrace";
    in flake // {
      defaultPackage = app;

      nixosModules.sonowz-raytrace =
        { config, lib, pkgs, ... }:
        let cfg = config.services.sonowz-raytrace;
        in
        {
          options.services.sonowz-raytrace = {
            enable = lib.mkEnableOption "sonowz-raytrace";
            options = lib.mkOption {
              description = "Command line arguments. '--help' to print options.";
              type = lib.types.str;
            };
          };

          config = lib.mkIf cfg.enable {
            systemd.services.sonowz-raytrace = {
              enable = true;
              path = [ app ];
              script = "sonowz-raytrace ${cfg.options}";
            };
          };
        };
    });
}