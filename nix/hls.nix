# This is a derivation for building HLS faster
{ lib, haskellPkgs, haskellLib }:
let
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
in
  lib.trivial.pipe haskellPkgs.haskell-language-server [
    fastBuild
    hls925Fix
    dynamicBuild
  ]
        