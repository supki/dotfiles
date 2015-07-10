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
    sha256 = "b6e0676ea7e85fcea5d67ec2c0e0dc50e9ff16afd1d25aeff67967160f775962";
    rev = "52b82af17b5cd50f25b2117f9d32e56de198f0db";
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
    base conduit conduit-extra containers data-default-class directory
    directory-layout filepath free hspec hspec-expectations-lens lens
    optparse-applicative process resourcet semigroups temporary text
    transformers unix
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
}
