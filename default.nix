{ mkDerivation, array, base, binary, bytestring, distributive
, intervals, JuicyPixels, lens, linear, mtl, mwc-random, QuickCheck
, semigroupoids, semigroups, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "panther";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    array base bytestring distributive intervals JuicyPixels lens
    linear mtl mwc-random semigroupoids semigroups
  ];
  testDepends = [
    base binary bytestring intervals lens linear QuickCheck tasty
    tasty-quickcheck
  ];
  license = stdenv.lib.licenses.bsd3;
}
