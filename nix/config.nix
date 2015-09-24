{
  packageOverrides = unusable: {

    # This thing practically cries for lenses.

    haskell = unusable.haskell // {
      packages = unusable.haskell.packages // {

        ghc784 = unusable.haskell.packages.ghc784.override {
          overrides = self: _: {
            hdevtools = self.callPackage ./hdevtools-7.8.nix {};
          };
        };

        ghc7102 = unusable.haskell.packages.ghc7102.override {
          overrides = self: _: {
            hdevtools = self.callPackage ./hdevtools-7.10.nix {};
          };
        } // (let
            ghc = unusable.haskell.packages.ghc7102;
            package = ghc.callPackage;
            derive = ghc.mkDerivation;
          in {
            biegunka = package ./biegunka.nix {
              mkDerivation = args: derive(args // {
                buildTools = (if args ? buildTools then args.buildTools else []) ++ [ unusable.git ];
              });
            };
            biegunka-svn = package ./biegunka-svn.nix {};
            pakej = package ./pakej.nix {};
          });
      };
    };

    bats = unusable.callPackage ./bats.nix {};
  };

  allowUnfree = true;
}
