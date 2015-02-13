{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellngPackages }:

let 
  hs = haskellPackages.override {
        overrides = self: super: rec {
          hsPkg = pkg: version: self.callPackage "/home/bergey/code/nixHaskellVersioned/${pkg}/${version}.nix" {};
          # required, not in Nix
          # version pins
          intervals = hsPkg "intervals" "0.7.1";
          # HEAD packages
          # self
          thisPackage = self.callPackage ./. {};
      };
    };
  in hs.thisPackage.env
