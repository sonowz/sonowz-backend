# This is a derivation for building HLS faster
{ lib, haskellPkgs, haskellLib }:
let
  fastHaskellPkgs = haskellPkgs.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { })) (hself: hsuper: {
      mkDerivation = args: hsuper.mkDerivation (args // {
        doHaddock = false;
        doCheck = false;
        doBenchmark = false;
        doCoverage = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
      });
    });
  });

  dynamicBuild = drv: lib.trivial.pipe drv [
    (haskellLib.compose.overrideCabal (old: {
      enableSharedExecutables = true;
    }))
    (haskellLib.compose.enableCabalFlag "dynamic")
  ];
in
  lib.trivial.pipe fastHaskellPkgs.haskell-language-server [
    dynamicBuild
  ]
        