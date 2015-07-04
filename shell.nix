{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc7101"
, biegunka ? ./nix/biegunka.nix
, biegunka-svn ? ./nix/biegunka-svn.nix
}: let
  inherit (nixpkgs) pkgs;
  everyone-dies = pkgs.haskell.packages.${compiler};
  ghc = everyone-dies.ghcWithPackages(ps: [
    ps.hdevtools alone afraid pakej
  ]);
  cabal-install = everyone-dies.cabal-install;
  cabal2nix = everyone-dies.cabal2nix;
  alone = everyone-dies.callPackage biegunka {
    mkDerivation = args: everyone-dies.mkDerivation(args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ pkgs.git ];
      doCheck = (biegunka == ./nix/biegunka.nix);
      doHaddock = false;
    });
  };
  pakej = everyone-dies.callPackage ./nix/pakej.nix {};
  afraid = everyone-dies.callPackage biegunka-svn {
    mkDerivation = args: everyone-dies.mkDerivation(args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ pkgs.subversion ];
      doCheck = (biegunka-svn == ./nix/biegunka-svn.nix);
      doHaddock = false;
    });
    biegunka = alone;
  };
in
  pkgs.stdenv.mkDerivation rec {
    name = "dotfiles";
    buildInputs = [ ghc cabal-install cabal2nix alone afraid pakej ];
    shellHook = ''
      export NIX_GHC="${ghc}/bin/ghc"
      export NIX_GHCPKG="${ghc}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${ghc}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR="${ghc}/lib/${ghc.name}"
      export IN_WHICH_NIX_SHELL=${name}
    '';
  }
