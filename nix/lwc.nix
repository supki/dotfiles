{ mkDerivation, async, base, bytestring, containers, fetchgit
, optparse-applicative, stdenv, stm, text
}:
mkDerivation {
  pname = "lwc";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/supki/lwc";
    sha256 = "41dcb40a16db4c11408fd22a0cad5697551390d563bee12a5a7ca38974c81f03";
    rev = "5bcef3e9a3d504ad49c3e90870bc6427a5a7137c";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base bytestring containers optparse-applicative stm text
  ];
  homepage = "https://github.com/supki/lwc";
  description = "Lazy wc(1)";
  license = stdenv.lib.licenses.bsd2;
}
