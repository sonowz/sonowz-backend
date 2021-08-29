with import <nixpkgs> {};

# This is no longer used for building
# Used for haskell-language-server environment (dynamic libraries)
# TODO: use nix-ld
stdenv.mkDerivation {
  name = "sonowz-backend";
  version = "1.0";
  src = ./bin;
  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [
    gmp
    libffi
    libpqxx
    ncurses
    zlib
    glibc
  ];
  LD_LIBRARY_PATH = "${glibc}/lib/";
  installPhase = ''
    mkdir -p $out/bin
    cp -r . $out/bin
  '';
  
}

