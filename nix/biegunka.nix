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
    sha256 = "83c38e25c665e707990cf716f019160b68e38f482e57cc33539fc96c46b785a2";
    rev = "18c1dc781f59435333ce569d3bef1d89030afd1a";
  };
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    acid-state aeson async base bytestring command-qq conduit
    conduit-extra containers cryptohash data-default-class directory
    directory-layout exceptions filepath free hspec HStringTemplate
    lens meep mtl optparse-applicative process resourcet safecopy
    semigroups stm template-haskell temporary text transformers unix
  ];
  testDepends = [
    base containers data-default-class directory directory-layout
    filepath free hspec hspec-expectations-lens lens
    optparse-applicative semigroups temporary text transformers unix
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
}
