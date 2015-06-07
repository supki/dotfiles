{ mkDerivation, base, cmdargs, directory, fetchgit, filepath, ghc
, ghc-paths, network, stdenv, syb, time, unix
}:
mkDerivation {
  pname = "hdevtools";
  version = "0.1.0.7";
  src = fetchgit {
    url = "https://github.com/supki/hdevtools";
    sha256 = "136ddc098337370434773c2b2d074d16e8ae6f17f0c070c927d7eca8b1051e70";
    rev = "d418ca16ce6903c36042b112de6e64ad5afe19ab";
  };
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base cmdargs directory filepath ghc ghc-paths network syb time unix
  ];
  homepage = "https://github.com/bitc/hdevtools/";
  description = "Persistent GHC powered background server for FAST haskell development tools";
  license = stdenv.lib.licenses.mit;
}
