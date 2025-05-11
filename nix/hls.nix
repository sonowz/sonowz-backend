# This is a derivation for building HLS faster
{ lib, haskellPkgs, haskellLib }:
let
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
    dynamicBuild
  ]
        