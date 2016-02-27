{ mkDerivation, ansi-terminal, base, base-compat, bytestring
, directory, fetchgit, filepath, fsnotify, hspec, hspec-wai
, http-client, http-types, interpolate, mockery, network, process
, silently, stdenv, stm, text, time, unix, wai, warp
}:
mkDerivation {
  pname = "sensei";
  version = "0.0.0";
  src = fetchgit {
    url = "https://github.com/hspec/sensei";
    sha256 = "b02741f40deac513a89119c2859f9b16a8bedcfbf2b957544a66ee25140bad07";
    rev = "9545f0a50fc5ae2f42cad4c2ac73ddf87ad01f46";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base base-compat bytestring directory filepath
    fsnotify http-client http-types network process stm text time unix
    wai warp
  ];
  executableHaskellDepends = [
    ansi-terminal base base-compat bytestring directory filepath
    fsnotify http-client http-types network process stm text time unix
    wai warp
  ];
  testHaskellDepends = [
    ansi-terminal base base-compat bytestring directory filepath
    fsnotify hspec hspec-wai http-client http-types interpolate mockery
    network process silently stm text time unix wai warp
  ];
  license = stdenv.lib.licenses.unfree;
}
