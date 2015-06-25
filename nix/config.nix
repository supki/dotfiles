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

        ghc7101 = unusable.haskell.packages.ghc7101.override {
          overrides = self: _: {
            hdevtools = self.callPackage ./hdevtools-7.10.nix {};
            biegunka = self.callPackage ./biegunka.nix {};
            pakej = self.callPackage ./pakej.nix {};
          };
        };
      };
    };
  };

  allowUnfree = true;
}
