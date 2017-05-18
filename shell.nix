{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bencoding, bytestring, containers
      , Crypto, HUnit, network, QuickCheck, stdenv, test-framework
      , test-framework-hunit, test-framework-quickcheck2
      }:
      mkDerivation {
        pname = "Mainline";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base ];
        executableHaskellDepends = [
          base bencoding bytestring containers Crypto network
        ];
        testHaskellDepends = [
          base bencoding bytestring containers Crypto HUnit QuickCheck
          test-framework test-framework-hunit test-framework-quickcheck2
        ];
        license = stdenv.lib.licenses.gpl2;
      };

  _haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages = _haskellPackages.override {
    overrides = self: super: {
      bencoding = self.callPackage ../bencoding/default.nix {};
    };
  };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv