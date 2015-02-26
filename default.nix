{ mkDerivation, array, base, binary, bytestring, distributive
, intervals, JuicyPixels, lens, linear, mtl, mwc-random, QuickCheck
, random, semigroupoids, semigroups, stdenv, tasty, tasty-hunit
, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "panther";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    array base bytestring distributive intervals JuicyPixels lens
    linear mtl mwc-random semigroupoids semigroups vector
  ];
  testDepends = [
    base binary bytestring intervals lens linear QuickCheck random
    semigroups tasty tasty-hunit tasty-quickcheck vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
