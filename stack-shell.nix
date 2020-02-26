{ ghc }:
with import <nixpkgs> {};

# Nix build environment is included for making images
haskell.lib.buildStackProject {
  inherit ghc;
  name = "sonowz-backend";
  # TODO: do not hard-code NIX_PATH
  NIX_PATH = "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos";
  buildInputs = [
    nix
    zlib
  ];
}

