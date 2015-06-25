{ mkDerivation, base, bytestring, cereal, cereal-conduit, conduit
, conduit-extra, directory, fetchgit, filepath, hashable, hspec
, hspec-expectations-lens, lens, netwire, network
, optparse-applicative, process, stdenv, text, transformers, unix
, unordered-containers
}:
mkDerivation {
  pname = "pakej";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/supki/pakej";
    sha256 = "c8dbfe43dfb8dfdc2b638c03e5b956c1fb968026fab2a4e35723899db0280c51";
    rev = "e09effc82ba7dac06e36bd0981bf7c0cf86526c7";
  };
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base bytestring cereal cereal-conduit conduit conduit-extra
    directory filepath hashable lens netwire network
    optparse-applicative process text transformers unix
    unordered-containers
  ];
  testDepends = [
    base bytestring cereal cereal-conduit conduit conduit-extra
    directory filepath hashable hspec hspec-expectations-lens lens
    netwire network optparse-applicative text transformers unix
    unordered-containers
  ];
  homepage = "http://example.com/";
  description = "Status bar daemon";
  license = stdenv.lib.licenses.bsd3;
}
