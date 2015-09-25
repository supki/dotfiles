{ mkDerivation, base, biegunka, directory, fetchgit, filepath
, hspec, hspec-expectations-lens, lens, process, stdenv, temporary
, transformers
}:
mkDerivation {
  pname = "biegunka-svn";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/biegunka/biegunka-svn";
    sha256 = "5d23ddd9e9d1b209b629381ae934e412354fb4f276b296b8dfd1cd968e7d20c1";
    rev = "9e605fb4cfc7292799960e3008b92538d990f566";
  };
  libraryHaskellDepends = [
    base biegunka directory lens process transformers
  ];
  testHaskellDepends = [
    base directory filepath hspec hspec-expectations-lens lens process
    temporary transformers
  ];
  homepage = "https://github.com/biegunka/biegunka-svn";
  description = "SVN support for Biegunka";
  license = stdenv.lib.licenses.bsd2;
}
