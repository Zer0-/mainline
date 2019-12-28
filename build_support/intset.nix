{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, deepseq
      , stdenv, cabal-install, test-framework-quickcheck2, criterion, QuickCheck
      , test-framework, containers
      }:
      mkDerivation {
        pname = "intset";
        version = "0.1.1.0";
        src = pkgs.fetchFromGitHub {
          owner = "Zer0-";
          repo = "intset";
          rev = "4f1c456a0bef08c5d89f59a2125138a47612a0d7";
          sha256 = "12bwkqqwqvw3pm5ra76s9bbx5nidslbh2sp2m69fjdahrx9cfb6b";
        };
        libraryHaskellDepends = [
          base
          bytestring
          deepseq
        ];
        testHaskellDepends = [
          base QuickCheck test-framework test-framework-quickcheck2 cabal-install
        ];
        benchmarkHaskellDepends = [
          base bytestring containers criterion deepseq
        ];
        homepage = "https://github.com/pxqr/intset";
        description = "Pure, mergeable, succinct Int sets";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in
  drv
  #if pkgs.lib.inNixShell then drv.env else drv
