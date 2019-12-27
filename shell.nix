{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  bittorrent = pkgs.callPackage ../sources/bittorrent/default.nix {};

  squeal-postgresql = pkgs.callPackage ../sources/squeal/shell.nix {};

  f = { mkDerivation, base, bencoding, bytestring, containers
      , Crypto, HUnit, network, QuickCheck, stdenv, test-framework
      , test-framework-hunit, test-framework-quickcheck2, crypto-api
      , DRBG, text, time, cabal-install, network-transport, cpu
      , parsec, lrucache, hex, cereal, data-default, bittorrent
      , squeal-postgresql, llvmPackages_6
      , safe-exceptions, unagi-chan
      }:
      mkDerivation {
        pname = "Mainline";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base ];
        executableHaskellDepends = [
          base bencoding bytestring containers Crypto network crypto-api
          DRBG text time cpu parsec lrucache cereal data-default bittorrent
          squeal-postgresql llvmPackages_6.llvm
          safe-exceptions unagi-chan
        ];
        testHaskellDepends = [
          base bencoding bytestring containers Crypto HUnit QuickCheck
          test-framework test-framework-hunit test-framework-quickcheck2
          cabal-install network-transport hex
        ];
        license = {
          fullName = "Server Side Public License";
          url = https://www.mongodb.com/licensing/server-side-public-license;
          free = true;
        };
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f
    {
      bittorrent = bittorrent;
      squeal-postgresql = squeal-postgresql;
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
