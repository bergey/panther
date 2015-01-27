{ mkDerivation, array, base, bytestring, distributive, JuicyPixels
, lens, linear, mtl, mwc-random, stdenv
}:
mkDerivation {
  pname = "ray";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    array base bytestring distributive JuicyPixels lens linear mtl
    mwc-random
  ];
  license = stdenv.lib.licenses.bsd3;
}
