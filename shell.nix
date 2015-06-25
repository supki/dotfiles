{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc7101"
, devel ? false
}: let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages(ps: [
    ps.hdevtools biegunka pakej
  ]);
  cabal-install = pkgs.haskell.packages.${compiler}.cabal-install;
  cabal2nix = pkgs.haskell.packages.${compiler}.cabal2nix;
  biegunka = pkgs.haskell.packages.${compiler}.callPackage (if devel then ../biegunka/biegunka.nix else ./nix/biegunka.nix) {
    mkDerivation = args: nixpkgs.pkgs.haskell.packages.${compiler}.mkDerivation(args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ nixpkgs.pkgs.git ];
        doCheck = !devel;
        doHaddock = false;
    });
  };
  pakej = pkgs.haskell.packages.${compiler}.callPackage ./nix/pakej.nix {};
in
  pkgs.stdenv.mkDerivation rec {
    name = "dotfiles";
    buildInputs = [ ghc cabal-install cabal2nix biegunka pakej ];
    shellHook = ''
      export NIX_GHC="${ghc}/bin/ghc"
      export NIX_GHCPKG="${ghc}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${ghc}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR="${ghc}/lib/${ghc.name}"
      export IN_WHICH_NIX_SHELL=${name}
    '';
  }
