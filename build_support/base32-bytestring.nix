{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cpu, criterion, hspec
      , QuickCheck, stdenv, cabal-install
      }:
      mkDerivation {
        pname = "base32-bytestring";
        version = "0.2.1.0";
        src = pkgs.fetchFromGitHub {
          owner = "Zer0-";
          repo = "base32-bytestring";
          rev = "adbc55e0adaa13048450f1494a7358a5133aaa66";
          sha256 = "0v4dl2bz5wfv1brj0996b259v4s799cbnv3350hsqzl4dhlab99g";
        };
        libraryHaskellDepends = [ base bytestring cpu ];
        testHaskellDepends = [ base bytestring hspec QuickCheck cabal-install];
        benchmarkHaskellDepends = [ base bytestring criterion ];
        homepage = "https://github.com/pxqr/base32-bytestring";
        description = "Fast base32 and base32hex codec for ByteStrings";
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
