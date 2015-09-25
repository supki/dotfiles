{ mkDerivation, acid-state, aeson, async, base, bytestring
, command-qq, conduit, conduit-extra, containers, cryptohash
, data-default-class, directory, directory-layout, exceptions
, fetchgit, filepath, free, hspec, hspec-expectations-lens
, HStringTemplate, lens, meep, mtl, optparse-applicative, process
, resourcet, safecopy, semigroups, stdenv, stm, template-haskell
, temporary, text, transformers, unix
}:
mkDerivation {
  pname = "biegunka";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/biegunka/biegunka";
    sha256 = "aad51535223f9ac7237a2d27ea7b4998466377edfd5ee43da4e6a1206e85a5f8";
    rev = "aa1515dfec51f6468475c13828b5119f4eb2b9d1";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state async base bytestring command-qq conduit conduit-extra
    containers cryptohash directory directory-layout exceptions
    filepath free hspec HStringTemplate lens meep mtl
    optparse-applicative process resourcet safecopy semigroups stm
    template-haskell temporary text transformers unix
  ];
  executableHaskellDepends = [
    aeson base bytestring conduit conduit-extra containers
    data-default-class directory filepath lens process resourcet text
    transformers unix
  ];
  testHaskellDepends = [
    base conduit conduit-extra containers data-default-class directory
    directory-layout filepath free hspec hspec-expectations-lens lens
    optparse-applicative process resourcet semigroups temporary text
    transformers unix
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
}
