{ mkDerivation, base, Cabal, cmdargs, directory, fetchgit, filepath
, ghc, ghc-boot, ghc-paths, network, process, stdenv, syb, time
, transformers, unix
}:
mkDerivation {
  pname = "hdevtools";
  version = "0.1.3.2";
  src = fetchgit {
    url = "https://github.com/supki/hdevtools";
    sha256 = "15jmvp4vyh6g2sx3dmyn5rhp4lgqjfilrzs1qmnxanh2hxjz4s9w";
    rev = "41dfd3a7dcb46c017a355b5999dc1c91be551285";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal cmdargs directory filepath ghc ghc-boot ghc-paths
    network process syb time transformers unix
  ];
  homepage = "https://github.com/hdevtools/hdevtools/";
  description = "Persistent GHC powered background server for FAST haskell development tools";
  license = stdenv.lib.licenses.mit;
}
