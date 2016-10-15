{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc801"
, biegunka-nix ? ./nix/biegunka.nix
}: let
  inherit (nixpkgs) pkgs;
  haskell = pkgs.haskell.packages.${compiler};

  ghc = haskell.ghcWithPackages(ps: [
    ps.hdevtools ps.hlint ps.ghc-mod biegunka
  ]);

  biegunka = haskell.callPackage biegunka-nix {
    mkDerivation = args: haskell.mkDerivation(args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ pkgs.git ];
      doCheck = (biegunka-nix == ./nix/biegunka.nix);
      doHaddock = false;
    });
  };
in
  pkgs.stdenv.mkDerivation rec {
    name = "dotfiles";
    buildInputs = [
      ghc
      haskell.cabal-install
      biegunka
    ];
    shellHook = ''
      export NIX_GHC="${ghc}/bin/ghc"
      export NIX_GHCPKG="${ghc}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${ghc}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR="${ghc}/lib/${ghc.name}"
      export IN_WHICH_NIX_SHELL=${name}
    '';
  }
