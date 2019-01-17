{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bencoding, bytestring, containers
      , Crypto, HUnit, network, cpu, QuickCheck, stdenv, test-framework
      , test-framework-hunit, test-framework-quickcheck2, crypto-api
      , DRBG, text, time, cabal-install
      }:
      mkDerivation {
        pname = "Mainline";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base ];
        executableHaskellDepends = [
          base bencoding bytestring containers Crypto network cpu crypto-api
          DRBG text time
        ];
        testHaskellDepends = [
          base bencoding bytestring containers Crypto cpu HUnit QuickCheck
          test-framework test-framework-hunit test-framework-quickcheck2
          cabal-install
        ];
        license = stdenv.lib.licenses.gpl2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
