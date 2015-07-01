{ mkDerivation, base, biegunka, directory, fetchgit, process
, stdenv, transformers
}:
mkDerivation {
  pname = "biegunka-svn";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/biegunka/biegunka-svn";
    sha256 = "b60a43970eec5681ae95cb6252ec2609cfac5beba200c2f30021c03f4505fd0c";
    rev = "b3cfd39118e961946430e847252588db4c434f27";
  };
  buildDepends = [ base biegunka directory process transformers ];
  homepage = "https://github.com/biegunka/biegunka-svn";
  description = "SVN support for Biegunka";
  license = stdenv.lib.licenses.bsd2;
}
