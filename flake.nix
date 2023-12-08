{
  description = "Sonowz Backend";

  inputs = {
    nixpkgs.url = "nixpkgs/23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        version = "1.0.0";
        name = "sonowz-backend";

        pkgs = nixpkgs.legacyPackages.${system};
        lib = nixpkgs.lib;
        haskellPkgs = pkgs.haskell.packages.ghc925; # The version should match with resolver!
        haskellLib = pkgs.haskell.lib;

        requirements = pkgs.callPackage ./requirements.nix {};
        devRequirements = with pkgs; [
          stack-wrapped
          cabal-install
          
          custom-hls
          haskellPkgs.ormolu
          hlint
        ];

        custom-hls = lib.trivial.pipe haskellPkgs.haskell-language-server [
          fastBuild
          hls925Fix
          dynamicBuild
        ];
        # https://github.com/NixOS/nixpkgs/blob/30d709aa1b9d620351fb457e6ed805a0d4908774/pkgs/development/haskell-modules/configuration-ghc-9.2.x.nix#L76
        hls925Fix = drv: haskellLib.compose.disableCabalFlag "fourmolu" (drv.override { hls-fourmolu-plugin = null; });
        fastBuild = drv: drv.overrideScope (hself: hsuper: {
          mkDerivation = args: hsuper.mkDerivation (args // {
            doHaddock = false;
            doCheck = false;
            doBenchmark = false;
            doCoverage = false;
            enableLibraryProfiling = false;
          });
        });
        dynamicBuild = drv: lib.trivial.pipe drv [
          (haskellLib.compose.overrideCabal (old: {
            enableSharedExecutables = true;
          }))
          (haskellLib.compose.enableCabalFlag "dynamic")
        ];

        # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          version = pkgs.stack.version;
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        # TODO: remove hardcoded path
        binPath = /home/sonowz/sonowz-backend/bin;
        
        # This app is an impure derivation for dev purpose
        # Use 'flake.nix' inside each package for production packages
        # Build command : nix build --impure
        app = pkgs.stdenv.mkDerivation {
          inherit name;
          inherit version;
          src = binPath;
          nativeBuildInputs = [];
          propagatedBuildInputs = requirements;
          installPhase = ''
            mkdir -p $out/bin
            cp -r . $out/bin
          '';
        };
      in
      {
        inherit app;
        defaultPackage = app;

        devShell = (pkgs.haskell.lib.buildStackProject {
          inherit name;
          inherit version;
          ghc = haskellPkgs.ghc;
          stack = stack-wrapped;
          buildInputs = requirements;
        }).overrideAttrs (final: prev: {
          buildInputs = prev.buildInputs ++ devRequirements;
        });
        /* devShell = pkgs.mkShell {
          buildInputs = devRequirements ++ requirements ++ [ stack-wrapped ghc ];
          # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.glibc ];
        }; */
      }
    );
}