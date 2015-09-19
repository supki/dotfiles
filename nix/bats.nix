{ stdenv, fetchgit }:

stdenv.mkDerivation {
  name = "bats-0.4.0";
  builder = builtins.toFile "builder.sh" ''
    #!/usr/bin/env bash

    source "$stdenv/setup"
    set -e

    cd "$src"
    ./install.sh "$out"
  '';
  src = fetchgit {
    url = https://github.com/sstephenson/bats;
    sha256 = "1kd0vklrxlhlcgd9cck7yrxxb9951d82whdycnq2higp0gglvl64";
    rev = "955309ab943ea157ded0c402df98b160bb45ff92";
  };
}

