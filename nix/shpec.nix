{ stdenv, fetchgit }:

stdenv.mkDerivation {
  name = "shpec-0.2.2";
  builder = builtins.toFile "builder.sh" ''
    #!/usr/bin/env bash

    source "$stdenv/setup"
    set -e

    cd "$src"
    PREFIX="$out" make install
  '';
  src = fetchgit {
    url = https://github.com/rylnd/shpec;
    sha256 = "1yj78787mk7lpfk34s01c7xklw0apr4kh3jsvhaiz3dflsp4jmfk";
    rev = "f540bf17ccae3750f03fd9b19ef564c1e4017849";
  };
}

