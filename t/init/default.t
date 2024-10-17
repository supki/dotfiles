This init file sets up a basic Haskell project.
First, we define variables that have reasonable defaults but can be overriden if needed.
{# SETUP #}
{% set
    resolver =
      coalesce(resolver, "lts-22.27")
    ghc =
      coalesce(ghc, "965")
    with-executable? =
      coalesce(with-executable?, false)

    author =
      { name: coalesce(author-name, "Matvey Aksenov")
      , email-domain: coalesce(author-email-domain, "@gmail.com")
      }
%}
{# NOOP #}
Then, we define variables that must be defined by `init` invocation.
{# SETUP #}
{% set
    package =
      { name: package-name
      }
%}
{# NOOP #}
Lastly, we define the derived variables.
{# SETUP #}
{% set
    author.email =
      concat([join(".", split(" ", lower-case(author.name))), "@", author.email-domain])
    year = 2024
    meta-module-name =
      join("_", split("-", package.name))
    env-var-name =
      upper-case(join("_", split("-", package.name)))
%}
{# FILE stack.yaml #}
resolver: {{ resolver }}
packages:
  - '.'
extra-deps:
allow-newer: true

{# FILE package.yaml #}
name:                {{ package.name }}
version:             1.0.0
synopsis:            This is {{ package.name }}
description:         See README.markdown
license:             BSD2
author:              {{ author.name }}
maintainer:          {{ author.email }}
copyright:           {{ year }} {{ author.name }}
extra-source-files:
  - README.markdown
  - CHANGELOG.markdown

custom-setup:
  dependencies:
    - base
    - Cabal
    - directory
    - filepath
    - process

dependencies:
  - base >= 4.7 && < 5

default-extensions:
  - ImportQualifiedPost
  - NoFieldSelectors
  - OverloadedStrings

library:
  dependencies:
    - envparse
  source-dirs:
    src
  other-modules:
    - Meta_{{ meta-module-name }}
    - Paths_{{ meta-module-name }}
  ghc-options:
    - -funbox-strict-fields
    - -Wall
    - -Wno-incomplete-uni-patterns
    - -Werror

{% if with-executable? %}
executables:
  {{ package-name }}:
    dependencies:
      - {{ package.name }}
    source-dirs:
      driver
    main:
      Main.hs
    ghc-options:
      - -Wall
      - -Wno-incomplete-uni-patterns
      - -Werror
      - -threaded
      - -with-rtsopts=-N
{% endif %}

tests:
  spec:
    dependencies:
      - {{ package.name }}
      - hspec
    source-dirs:
      test
    main:
      Spec.hs
    ghc-options:
      - -freduction-depth=0
      - -Wall
      - -Wno-incomplete-uni-patterns
      - -Werror
      - -threaded
      - -with-rtsopts=-N

{# FILE Setup.hs #}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Data.Char qualified as Char
import Data.List qualified as List
import Distribution.Package (PackageIdentifier(..), unPackageName)
import Distribution.PackageDescription (PackageDescription(package))
import Distribution.Pretty (prettyShow)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, buildHook)
import Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Distribution.Simple.Setup (BuildFlags(buildVerbosity), fromFlag)
import Distribution.Simple.Utils (notice, rewriteFileEx)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.IO.Error (catchIOError)
import System.Process (readProcess)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pd lbi uh bf -> do generateMeta lbi bf; buildHook simpleUserHooks pd lbi uh bf
  }

generateMeta :: LocalBuildInfo -> BuildFlags -> IO ()
generateMeta lbi bf = let
    verbosity = fromFlag (buildVerbosity bf)
    PackageIdentifier {pkgName, pkgVersion} = package (localPkgDescr lbi)
    metaName = "Meta_" ++ replace '-' '_' (unPackageName pkgName)
    metaPath = autogen </> metaName <.> "hs"
  in do
    hash <- gitHash
    createDirectoryIfMissing True autogen
    notice verbosity ("Generating " ++ metaPath ++ " ...")
    rewriteFileEx verbosity metaPath (unlines
      [ "module " ++ metaName
      , "  ( name"
      , "  , version"
      , "  ) where"
      , ""
      , "import Data.String (IsString(fromString))"
      , ""
      , "name :: IsString str => str"
      , "name = fromString " ++ show (unPackageName pkgName)
      , ""
      , "version :: IsString str => str"
      , "version = fromString " ++ show (prettyShow pkgVersion ++ "-" ++ hash)
      ])
 where
  autogen = autogenPackageModulesDir lbi
  replace x y =
    map (\c -> if c == x then y else c)

gitHash :: IO String
gitHash =
  catchIOError (fmap sanitize (readProcess "git" ["describe", "--always", "--dirty=-dirty"] ""))
               (\_ -> return "unknown")
 where
  sanitize = List.dropWhileEnd Char.isSpace

{# FILE README.markdown #}
{{ package.name }}
===

{# FILE CHANGELOG.markdown #}
1.0.0
=====

  * Initial release.

{# FILE LICENSE #}
Copyright {{ author.name }} (c) {{ year }}

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE..

{# FILE driver/Main.hs #}
module Main (main) where

import qualified App
import qualified Cfg


main :: IO ()
main = do
  cfg <- Cfg.get
  App.run cfg

{# FILE src/App.hs #}
module App
  ( run
  ) where

import Cfg (Cfg)


run :: Cfg -> IO ()
run _ =
  pure ()

{# FILE src/Cfg.hs #}
module Cfg
  ( Cfg(..)
  , get
  , Meta.version
  ) where

import           Env
import qualified Meta_{{ meta-module-name }} as Meta


data Cfg = Cfg
    deriving (Show, Eq)

get :: IO Cfg
get =
  Env.parse (header usageHeader) . prefixed "{{ env-var-name }}_" $ do
    pure Cfg

usageHeader :: String
usageHeader =
  unwords [Meta.name, Meta.version]

{# FILE test/Spec.hs #}
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

{# FILE shell.nix #}
{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.haskell.compiler.ghc{{ ghc }}
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
    glibcLocales
    gmp
    stack
    zlib
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}"
  '';
}

{# FILE .ghci #}
:set -Wall
:set -Wno-incomplete-uni-patterns
:set -isrc
:set -itest
:set -idriver
:set -i.stack-work/dist/x86_64-linux/ghc-{{ join(".", chunks-of(1, ghc)) }}/build/global-autogen
:set -XImportQualifiedPost
:set -XNoFieldSelectors
:set -XOverloadedStrings

{# FILE .gitignore #}
.stack-work/
.ghci_history
