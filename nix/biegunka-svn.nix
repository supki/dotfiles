{ mkDerivation, base, biegunka, directory, fetchgit, process
, stdenv, transformers
}:
mkDerivation {
  pname = "biegunka-svn";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/biegunka/biegunka-svn";
    sha256 = "6b54cdf68e2cf14e04b622e0734ba53c32e4a25cc1ef9f171a6551a32363dc02";
    rev = "e3287f639f057c89047afd56a0fc8e350115f240";
  };
  buildDepends = [ base biegunka directory process transformers ];
  homepage = "https://github.com/biegunka/biegunka-svn";
  description = "SVN support for Biegunka";
  license = stdenv.lib.licenses.bsd2;
}
