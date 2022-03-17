{
  description = "Sonowz Backend";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        version = "1.0.0";
        name = "sonowz-backend";

        pkgs = nixpkgs.legacyPackages.${system};
        requirements = pkgs.callPackage ./requirements.nix {};

        # TODO: remove hardcoded path
        binPath = /home/sonowz/sonowz-backend/bin;
        
        # This app is an impure derivation for dev purpose
        # Use 'flake.nix' inside each package for production packages
        # Build command : nix build --impure
        app = pkgs.stdenv.mkDerivation {
          inherit name;
          inherit version;
          src = binPath;
          nativeBuildInputs = [ pkgs.autoPatchelfHook ];
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

        devShell = pkgs.mkShell {
          # buildInputs = requirements;
          LD_LIBRARY_PATH = "${pkgs.glibc}/lib/";
        };
      }
    );
}