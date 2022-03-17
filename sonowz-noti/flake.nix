{
  description = "Notification generator";

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
      app = flake.packages."sonowz-noti:exe:sonowz-noti";
    in flake // {
      defaultPackage = app;

      nixosModules.sonowz-noti =
        { config, lib, pkgs, ... }:
        let cfg = config.services.sonowz-noti;
        in
        {
          options.services.sonowz-noti = {
            enable = lib.mkEnableOption "sonowz-noti";
            options = lib.mkOption {
              description = "Command line arguments. '--help' to print options.";
              type = lib.types.str;
            };
          };

          config = lib.mkIf cfg.enable {
            systemd.services.sonowz-noti = {
              enable = true;
              path = [ app ];
              script = "sonowz-noti ${cfg.options}";
            };
          };
        };
    });
}