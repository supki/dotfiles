{ mkDerivation, base, biegunka, directory, fetchgit, filepath
, hspec, hspec-expectations-lens, lens, process, stdenv, temporary
, transformers
}:
mkDerivation {
  pname = "biegunka-svn";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/biegunka/biegunka-svn";
    sha256 = "602de4a07843ae9f30a5814c07baa0929a5d150b8786474069b2dc7eab37a1c7";
    rev = "cd9c1d46dac750a506a3ca66ab07f20087ef8b75";
  };
  buildDepends = [
    base biegunka directory lens process transformers
  ];
  testDepends = [
    base directory filepath hspec hspec-expectations-lens lens process
    temporary transformers
  ];
  homepage = "https://github.com/biegunka/biegunka-svn";
  description = "SVN support for Biegunka";
  license = stdenv.lib.licenses.bsd2;
}
