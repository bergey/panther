{ mkDerivation, array, base, binary, bytestring, distributive
, JuicyPixels, lens, linear, mtl, mwc-random, QuickCheck, stdenv
, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "panther";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    array base bytestring distributive JuicyPixels lens linear mtl
    mwc-random
  ];
  testDepends = [
    base binary bytestring QuickCheck tasty tasty-quickcheck
  ];
  license = stdenv.lib.licenses.bsd3;
}
