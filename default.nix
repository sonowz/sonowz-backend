with import <nixpkgs> {};

# This derivation just copies binaries from (impure) stack build result.
# Use 'stack install --local-bin-path=bin' to build binaries
stdenv.mkDerivation {
  name = "sonowz-backend";
  version = "1.0";
  src = ./bin;
  installPhase = ''
    mkdir -p $out/bin
    cp sonowz-raytrace $out/bin
  '';
}

