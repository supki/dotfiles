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
    sha256 = "87f57df6cf3325a5c489035aa4a32eca16c009730228acec1c08d7853c1d7c48";
    rev = "647667031917bba4b4854e56e7e78ba8b16c14b1";
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
