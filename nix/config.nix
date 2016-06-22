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

        ghc7103 = unusable.haskell.packages.ghc7103.override {
          overrides = self: _: {
            hdevtools = self.callPackage ./hdevtools-7.10.nix {};
          };
        } // (let
            ghc = unusable.haskell.packages.ghc7103;
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
            sensei = package ./sensei.nix {};
            lwc = package ./lwc.nix {};
          });
      };
    };

    bats = unusable.callPackage ./bats.nix {};
    shpec = unusable.callPackage ./shpec.nix {};
  };

  # When working with an unfree Haskell package, instead of allowing unfree packages everywhere:
  #
  # allowUnfree = true;
  #
  # it's possible to import the package's `default.nix` with a bespoken `nixpkgs` import:
  #
  # this = (import ./default.nix {
  #   inherit compiler;
  #   nixpkgs = import <nixpkgs> {
  #     config.allowUnfree = true;
  #   };
  # });
  #
  # Doing this at the top level without repeating the import won't work though, `hdevtools`
  # inexplicably stops working at all, complaining about missing dependencies.
}
