{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  free-categories = pkgs.callPackage ./free-categories.nix {};

  setSourceRoot = dir: drv: drv.overrideAttrs (_oldAttrs: {sourceRoot = "source/${dir}";});

  f = { mkDerivation, aeson, base, binary-parser, bytestring
      , bytestring-strict-builder, deepseq, doctest, generics-sop, hspec
      , mmorph, mtl, network-ip, postgresql-binary, postgresql-libpq
      , records-sop, resource-pool, scientific, stdenv, text, time
      , transformers, unliftio, unliftio-pool, uuid-types, vector
      , cabal-install, free-categories, base16-bytestring
      }:
      mkDerivation {
        pname = "squeal-postgresql";
        version = "0.5.1.0";
        src = pkgs.fetchFromGitHub {
          owner = "Zer0-";
          repo = "squeal";
          rev = "38cd2850407a85f1637340e16addb0512bc50091";
          sha256 = "16282xgh4vx9zz0xm5ini7xsvkrqkjjah8kz7fs6y387486cq1p1";
        };
        isLibrary = true;
        isExecutable = false;
        libraryHaskellDepends = [
          aeson base binary-parser bytestring bytestring-strict-builder
          deepseq generics-sop mmorph mtl network-ip postgresql-binary
          postgresql-libpq records-sop resource-pool scientific text time
          transformers unliftio unliftio-pool uuid-types vector
          free-categories base16-bytestring
          cabal-install
        ];
        executableHaskellDepends = [
          base bytestring generics-sop mtl text transformers vector
        ];
        testHaskellDepends = [
          base bytestring doctest generics-sop hspec text transformers vector
          cabal-install
        ];
        doCheck = false;
        homepage = "https://github.com/morphismtech/squeal";
        description = "Squeal PostgreSQL Library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv =
    setSourceRoot
    "squeal-postgresql"
    (variant (haskellPackages.callPackage f {
      free-categories = free-categories;
    }));

in
  drv
  #if pkgs.lib.inNixShell then drv.env else drv
