{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv }:
      mkDerivation {
        src = pkgs.fetchFromGitHub {
          owner = "morphismtech";
          repo = "free-categories";
          rev = "21f31b9b0c1bd20d5b02b967c64e64921d5885d3";
          sha256 = "11kywr6g51alh83mpghsn3q635spmppyhci6lcywfb0zcbc7h1sa";
        };
        pname = "free-categories";
        version = "0.1.0.0";
        sha256 = "0lzal6vbh1zjcag4dwmhnsv4j66n00gkl0cmf0pssdjwwywxgpwx";
        libraryHaskellDepends = [ base ];
        homepage = "http://github.com/morphismtech/free-categories";
        description = "free categories";
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
