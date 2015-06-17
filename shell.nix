{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }: let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages(ps: with ps; [
    hdevtools biegunka
  ]);
  cabal-install = pkgs.haskell.packages.${compiler}.cabal-install;
  biegunka = pkgs.haskell.packages.${compiler}.biegunka;
in
  pkgs.stdenv.mkDerivation rec {
    name = "dotfiles";
    buildInputs = [ ghc cabal-install biegunka ];
    shellHook = ''
      export NIX_GHC="${ghc}/bin/ghc"
      export NIX_GHCPKG="${ghc}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${ghc}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR="${ghc}/lib/${ghc.name}"
      export IN_WHICH_NIX_SHELL=${name}
    '';
  }
