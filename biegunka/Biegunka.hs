{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Control.Lens
import           Data.Foldable (traverse_)
import           System.FilePath (combine)
import           Text.Printf (printf)

import           Control.Biegunka
import           Control.Biegunka.Source (Url, HasUrl(url), path)
import           Control.Biegunka.Source.Git (git, branch)
import qualified Control.Biegunka.Source.Git as Git
import           Control.Biegunka.Templates.HStringTemplate

import qualified Laptop
import qualified Work


data E = Laptop | Work deriving (Generic)

instance Environments E


main :: IO ()
main = do
  (environment, runBiegunka) <- runnerOf
  let settings ts = set templates (hStringTemplate ts)
  case environment of
    Laptop -> runBiegunka (settings Laptop.template) laptop
    Work   -> runBiegunka (settings Work.template) work

laptop, work :: Script 'Sources ()
laptop = sudo "maksenov" $ sequence_
  [ dotfiles
  , tools
  , vim
  , emacs
  , misc
  , mine
  ]
work = sequence_
  [ dotfiles
  , vim
  , misc
  , mine
  ]

dotfiles :: Script 'Sources ()
dotfiles = namespace "dotfiles" $
  github "supki" ".dotfiles" (path "git/dotfiles") $ do
    traverse_ (uncurry link)
              (concat [core, extended, script])
    traverse_ (uncurry substitute)
              template
    nix
    [sh|DISPLAY=:0 xrdb -merge ~/.Xresources|]
    [sh|xmonad --recompile|]
    [sh|pakej --recompile|]
    let pathogen, diff_highlight :: String
        pathogen = "https://raw.githubusercontent.com/tpope/vim-pathogen/b4174e4d1a16e7f0c5c6dfca923269a20a9f50c2/autoload/pathogen.vim"
        diff_highlight = "https://raw.githubusercontent.com/git/git/8d530c4d64ffcc853889f7b385f554d53db375ed/contrib/diff-highlight/diff-highlight"
    [sh|
      mkdir -p  ~/.vim/{autoload,backups,bundle,colors,ftdetect,syntax,temp,undo}
      curl --fail --location #{pathogen} -o ~/.vim/autoload/pathogen.vim
      curl --fail --location #{diff_highlight} -o ~/bin/diff-highlight
      chmod +x ~/bin/diff-highlight
    |]
    [sh|lesskey|]
    [sh|chmod +x ~/.xsessionrc|]
    [sh|chmod '0755' "${SOURCE_ROOT}/core/"; chmod '0644' "${SOURCE_ROOT}/core/ghci"|]
    [sh|
       cabal2nix "https://github.com/supki/pakej" > "nix/pakej.nix"
       cabal2nix "https://github.com/biegunka/biegunka" > "nix/biegunka.nix"
       cabal2nix "https://github.com/biegunka/biegunka-svn" > "nix/biegunka-svn.nix"
       cabal2nix "https://github.com/hspec/sensei" > "nix/sensei.nix"
       cabal2nix "https://github.com/supki/lwc" > "nix/lwc.nix"
    |]
 where
  core = over (mapped._1) (combine "core")
    [ dot "mpdconf"
    , dot "profile"
    , dot "zshenv"
    , dot "zshrc"
    , dot "inputrc"
    , dot "vimrc"
    , dot "ghci"
    , dot "irbrc"
    , dot "haskeline"
    , dot "racketrc"
    , dot "gitconfig"
    , dot "gitignore"
    , dot "XCompose"
    , dot "vimusrc"
    , dot "tmux.conf"
    , dot "emacs"
    , dot "sqliterc"
    , dot "pythonrc"
    , dot "curlrc"
    , dot "guard.rb"
    , dot "vim-jinjing"
    , dot "psqlrc"
    , dot "lesskey"
    , dot "colordiffrc"
    , "vim/vim.custom"                    ~> ".vim/plugin/vimrc-local.vim"
    , "vim/indent/haskell.vim"            ~> ".vim/indent/haskell.vim"
    , "vim/ftdetect/json.vim"             ~> ".vim/ftdetect/json.vim"
    , "vim/syntax/json.vim"               ~> ".vim/syntax/json.vim"
    , "vim/templates"                     ~> ".vim/templates"
    , "pakej.hs"                          ~> ".pakej/pakej.hs"
    , "transmission-daemon/settings.json" ~> ".transmission-daemon/settings.json"
    , "profile"                           ~> ".xmonad/xmonad-session-rc"
    , "profile"                           ~> ".zprofile"
    , "docker/config.json"                ~> ".docker/config.json"
    , "gpg-agent.conf"                    ~> ".gnupg/gpg-agent.conf"
    ]
  extended = over (mapped._1) (combine "extended")
    [ "xmonad.hs"                ~> ".xmonad/xmonad.hs"
    , "xmonad/Bindings.hs"       ~> ".xmonad/lib/Bindings.hs"
    , "xmonad/Startup.hs"        ~> ".xmonad/lib/Startup.hs"
    , "xmonad/Themes.hs"         ~> ".xmonad/lib/Themes.hs"
    , "xmonad/Tmux.hs"           ~> ".xmonad/lib/Tmux.hs"
    , "xmonad/Workspaces.hs"     ~> ".xmonad/lib/Workspaces.hs"
    , "xmonad/Spawn.hs"          ~> ".xmonad/lib/Spawn.hs"
    , "xmonad/PackagePrompt.hs"  ~> ".xmonad/lib/PackagePrompt.hs"
    , "parcelliterc"             ~> ".config/parcellite/parcelliterc"
    , "zathurarc"                ~> ".config/zathura/zathurarc"
    ]
  template = over (mapped._1) (combine "template")
    [ "xsession"                 ~> ".xsession"
    , "xsession"                 ~> ".xsessionrc"
    , "xmonad/Profile.hs"        ~> ".xmonad/lib/Profile.hs"
    , "xmodmap"                  ~> ".xmodmap"
    , "Xresources"               ~> ".Xresources"
    ]
  script = over (mapped._1) (combine "script")
    [ bin "whereami"
    , bin "so?"
    ]
  nix = do
    copy "nix/config.nix"         ".nixpkgs/config.nix"
    link "nix/hdevtools-7.8.nix"  ".nixpkgs/hdevtools-7.8.nix"
    link "nix/hdevtools-7.10.nix" ".nixpkgs/hdevtools-7.10.nix"
    link "nix/biegunka.nix"       ".nixpkgs/biegunka.nix"
    link "nix/biegunka-svn.nix"   ".nixpkgs/biegunka-svn.nix"
    link "nix/pakej.nix"          ".nixpkgs/pakej.nix"
    link "nix/bats.nix"           ".nixpkgs/bats.nix"
    link "nix/shpec.nix"          ".nixpkgs/shpec.nix"
    link "nix/sensei.nix"         ".nixpkgs/sensei.nix"
    link "nix/lwc.nix"            ".nixpkgs/lwc.nix"

tools :: Script 'Sources ()
tools = namespace "tools" $
  git (url "git@budueba.com:tools" . path "git/tools") $ do
    suid_binaries & unzipWithM_ (\s t ->
      sudo "root" $ [sh|
        ghc -O #{s} -fforce-recomp -threaded -v0 -o #{t}
        chown root:root #{t}
        chmod +s #{t}
      |])
    user_binaries & unzipWithM_ (\s t -> do
      [sh|ghc -O #{s} -fforce-recomp -v0 -o #{t}|]
      link t (combine "bin" t))
    unzipWithM_ link scripts
 where
  scripts, user_binaries, suid_binaries :: [(String, String)]
  scripts =
    [ "youtube-in-mplayer.sh" ~> "bin/youtube-in-mplayer"
    , "cue2tracks.sh"         ~> "bin/cue2tracks"
    , "pemised.rb"            ~> "bin/pemised"
    , "upload/screenshot.sh"  ~> "bin/upload-screenshot"
    , "pretty-json.py"        ~> "bin/pretty-json"
    , "publish-haddocks.sh"   ~> "bin/publish-haddocks"
    , "vaio-audio"            ~> "bin/vaio-audio"
    , "vaio-touchpad"         ~> "bin/vaio-touchpad"
    ]
  user_binaries =
    [ "audio.hs"              ~> "vaio-audio"
    ]
  suid_binaries =
    [ "vaio/touchpad.hs"      ~> "vaio-touchpad"
    ]

vim :: Script 'Sources ()
vim = do
  namespace "vim" $ do
    namespace "haskell" $ do
      pathogen  (github "Shougo" "vimproc") $
        [sh|make -f make_unix.mak|]
      pathogen_ (github "eagletmt" "ghcmod-vim")
      pathogen_ (github "ujihisa" "neco-ghc")
      pathogen_ (github "Shougo" "neocomplcache")
      pathogen_ (github "bitc" "vim-hdevtools")
    namespace "coq" $ do
      pathogen_ (github "vim-scripts" "coq-syntax")
      pathogen_ (github "vim-scripts" "Coq-indent")
      pathogen_ (github "trefis" "coquille")
    namespace "misc" $ do
      pathogen_ (github "scrooloose" "syntastic")
      pathogen_ (github "tpope" "vim-commentary")
      pathogen_ (github "tpope" "vim-unimpaired")
      pathogen_ (github "def-lkb" "vimbufsync")
      pathogen_ (github "ivyl" "vim-bling")
      pathogen_ (github "kien" "rainbow_parentheses.vim")
      pathogen_ (github "junegunn" "fzf.vim")
      github "junegunn" "fzf" (path (into "git")) $
        [sh|./install --bin|]
      pathogen_ (github "vim-airline" "vim-airline")
      pathogen_ (github "vim-airline" "vim-airline-themes")
      pathogen_ (github "stephpy" "vim-yaml")
      pathogen_ (github "roman" "golden-ratio")
      pathogen_ (github "markcornick" "vim-bats")
      pathogen_ (github "machakann" "vim-sandwich")
      pathogen_ (github "junegunn" "vim-easy-align")
      pathogen_ (github "ap" "vim-css-color")
      pathogen_ (github "aperezdc" "vim-template")
      pathogen_ (github "exu" "pgsql.vim")
      pathogen_ (github "tpope" "vim-eunuch")
    namespace "idris" $
      pathogen_ (github "idris-hackers" "idris-vim")
    namespace "rust" $
      pathogen_ (github "wting" "rust.vim")
    namespace "purescript" $
      pathogen_ (github "supki" "purescript-vim")
    namespace "javascript" $ do
      pathogen_ (github "pangloss" "vim-javascript")
      pathogen_ (github "mxw" "vim-jsx")
      pathogen_ (github "mhinz" "vim-signify")
    namespace "mine" $ do
      github "supki" "vim-flipping" (path (into "git")) $
        register ".vim/bundle/vim-flipping"
      github "supki" "vim-languages" (path (into "git")) $
        register ".vim/bundle/vim-languages"
      github "supki" "seoul256.vim" (path (into ".vim/bundle") . branch "f/m") pass
      pathogen_ (github "supki" "haskell-vim")
  namespace "vimish" $
    namespace "haskell" $
      pathogen_ (github "bitc" "hdevtools")
 where
  pathogen  g = g (path (into ".vim/bundle"))
  pathogen_ g = pathogen g pass

emacs :: Script 'Sources ()
emacs = namespace "emacs" $ do
  namespace "colorschemes" $ do
    github "bbatsov" "zenburn-emacs" (path (into "git/emacs")) $
      copy "zenburn-theme.el" ".emacs.d/themes/zenburn-theme.el"
    github "fommil" "darcula-theme-emacs" (path (into "git/emacs")) $
      copy "darcula-theme.el" ".emacs.d/themes/darcula-theme.el"
  namespace "usable" $ do
    github "emacsmirror" "paredit" (path (into "git/emacs")) $
      copy "paredit.el" ".emacs.d/plugins/paredit.el"
    github "jlr" "rainbow-delimiters" (path (into "git/emacs")) $
      copy "rainbow-delimiters.el" ".emacs.d/plugins/rainbow-delimiters.el"

misc :: Script 'Sources ()
misc =
  namespace "misc" $ do
    traverse_ (\g -> g (path (into "git")) pass)
      [ github "zsh-users" "zsh-syntax-highlighting"
      , github "zsh-users" "zsh-autosuggestions"
      , github "muennich" "urxvt-perls"
      ]
    github "so-fancy" "diff-so-fancy" (path (into "git")) $
      link "diff-so-fancy" "bin/diff-so-fancy"

mine :: Script 'Sources ()
mine = namespace "mine" $ do
  traverse_ (\p -> github "supki" p (path (into "git")) pass)
    [ "xmonad-screenshot"
    , "xmonad-2014"
    , "pakej"
    , "da"
    ]
  github "supki" "slock" (path (into "git") . branch "feature/usable") $
    pure ()

dot :: FilePath -> (FilePath, FilePath)
dot fp = fp ~> ('.' : fp)

bin :: FilePath -> (FilePath, FilePath)
bin fp = fp ~> combine "bin" fp

unzipWithM_ :: Applicative m => (a -> b -> m c) -> [(a, b)] -> m ()
unzipWithM_ = traverse_ . uncurry

github :: HasUrl s (Git.Config Url FilePath) Url => String -> String -> (Git.Config _ _ -> s) -> Script 'Actions () -> Script 'Sources ()
github user project f = git (url (printf "git@github.com:%s/%s" user project) . f)
