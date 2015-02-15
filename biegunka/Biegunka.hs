{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main (main) where

import           Control.Applicative
import           Control.Lens
import           Data.Default.Class (def)
import           Data.Foldable (traverse_)
import           System.FilePath ((</>))
import           Text.Printf (printf)

import           Control.Biegunka
import           Control.Biegunka.Source.Git
import           Control.Biegunka.Templates.HStringTemplate

import qualified Laptop
import qualified Work


data Environments = Laptop | Work deriving (Typeable, Data)


main :: IO ()
main = do
  (environment, runBiegunka) <- options [Laptop, Work]
  let settings ts = set root "~" . set templates (hStringTemplate ts)
  case environment of
    Laptop -> runBiegunka (settings Laptop.templates) laptop
    Work   -> runBiegunka (settings Work.templates) work

laptop, work :: Script Sources ()
laptop = sudo "maksenov" $ sequence_
  [ dotfiles
  , tools
  , vim
  , emacs
  , misc
  , experimental
  , edwardk
  , mine
  ]
work = sequence_
  [ dotfiles
  , vim
  , misc
  , experimental
  , mine
  ]


dotfiles = role "dotfiles" $
  git (github "supki" ".dotfiles") "git/dotfiles" $ do
    cores     & mapped._1 <\>~ "core"     & unzipWithM_ link
    extendeds & mapped._1 <\>~ "extended" & unzipWithM_ link
    templates & mapped._1 <\>~ "template" & unzipWithM_ substitute
    miscs     & mapped._1 <\>~ "misc"     & unzipWithM_ link
    [sh|xrdb -merge ~/.Xdefaults|]
    [sh|xmonad --recompile|]
    [sh|pakej --recompile|]
    let pathogen_url :: String
        pathogen_url = "https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim"
    [sh|
      mkdir -p  ~/.vim/autoload ~/.vim/bundle ~/.vim/colors
      curl -o ~/.vim/autoload/pathogen.vim #{pathogen_url}
    |]
    [sh|lesskey|]
    [sh|chmod +x ~/.xsessionrc|]
 where
  cores =
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
    , dot "ackrc"
    , dot "XCompose"
    , dot "vimusrc"
    , dot "tmux.conf"
    , dot "emacs"
    , dot "poneaux.rb"
    , dot "sqliterc"
    , dot "pythonrc"
    , dot "curlrc"
    , dot "codorc"
    , dot "guardrc"
    , dot "guard.rb"
    , dot "vim-jinjing"
    , dot "psqlrc"
    , dot "lesskey"
    , dot "colordiffrc"
    , "vim/vim.custom"                    ~> ".vim/plugin/vimrc-local.vim"
    , "vim/indent/haskell.vim"            ~> ".vim/indent/haskell.vim"
    , "vim/camo.vim"                      ~> ".vim/colors/camo.vim"
    , "vim/zenburn.vim"                   ~> ".vim/colors/zenburn.vim"
    , "pakej.hs"                          ~> ".pakej/pakej.hs"
    , "vifmrc"                            ~> ".vifm/vifmrc"
    , "transmission-daemon/settings.json" ~> ".transmission-daemon/settings.json"
    , "profile"                           ~> ".xmonad/xmonad-session-rc"
    ]
  extendeds =
    [ dot "gvimrc"
    , dot "pentadactylrc"
    , dot "gtkrc.mine"
    , dot "readme-gen.css"
    , "xmonad.hs"                ~> ".xmonad/xmonad.hs"
    , "xmonad/Bindings.hs"       ~> ".xmonad/lib/Bindings.hs"
    , "xmonad/Startup.hs"        ~> ".xmonad/lib/Startup.hs"
    , "xmonad/Themes.hs"         ~> ".xmonad/lib/Themes.hs"
    , "xmonad/Route.hs"          ~> ".xmonad/lib/Route.hs"
    , "xmonad/Tmux.hs"           ~> ".xmonad/lib/Tmux.hs"
    , "xmonad/Workspaces.hs"     ~> ".xmonad/lib/Workspaces.hs"
    , "xmonad/Spawn.hs"          ~> ".xmonad/lib/Spawn.hs"
    , "xmonad/PackagePrompt.hs"  ~> ".xmonad/lib/PackagePrompt.hs"
    , "pentadactyl/wanker.penta" ~> ".pentadactyl/plugins/wanker.penta"
    , "mplayer-config"           ~> ".mplayer/config"
    ]
  templates =
    [ "xsession"                 ~> ".xsession"
    , "xsession"                 ~> ".xsessionrc"
    , "xmonad/Profile.hs"        ~> ".xmonad/lib/Profile.hs"
    , "xmodmap"                  ~> ".xmodmap"
    , "Xdefaults"                ~> ".Xdefaults"
    ]
  miscs =
    [ bin "bat.rb"
    , bin "ip.awk"
    , bin "weather.rb"
    , bin "dive-into-mail"
    , bin "svn-browse"
    , bin "whereami"
    ]

tools = role "tools" $
  git "git@budueba.com:tools" "git/tools" $ do
    suid_binaries & unzipWithM_ (\s t ->
      sudo "root" $ [sh|
        ghc -O #{s} -fforce-recomp -threaded -v0 -o #{t}
        chown root:root #{t}
        chmod +s #{t}
      |])
    user_binaries & unzipWithM_ (\s t -> do
      [sh|ghc -O #{s} -fforce-recomp -v0 -o #{t}|]
      link t ("bin" </> t))
    scripts & unzipWithM_ link
 where
  scripts, user_binaries, suid_binaries :: [(String, String)]
  scripts =
    [ "youtube-in-mplayer.sh" ~> "bin/youtube-in-mplayer"
    , "cue2tracks.sh"         ~> "bin/cue2tracks"
    , "mpd/.lastfm.conf"      ~> ".lastfm.conf"
    , "mpd/lastfm.png"        ~> ".icons/lastfm.png"
    , "mpd/love.hs"           ~> "bin/lastfm-love-current-mpd-track"
    , "pemised.rb"            ~> "bin/pemised"
    , "upload/screenshot.sh"  ~> "bin/upload-screenshot"
    , "upload/budueba.sh"     ~> "bin/upload-budueba"
    , "isup.sh"               ~> "bin/isup"
    , "pretty-json.py"        ~> "bin/pretty-json"
    , "publish-haddocks.sh"   ~> "bin/publish-haddocks"
    , "vaio-audio"            ~> "bin/vaio-audio"
    , "vaio-touchpad"         ~> "bin/vaio-touchpad"
    , "suspender"             ~> "bin/suspender"
    ]
  user_binaries =
    [ "audio.hs"              ~> "vaio-audio"
    , "jenkins-hi.hs"         ~> "jenkins-hi"
    , "playcount.hs"          ~> "playcount"
    ]
  suid_binaries =
    [ "suspender.hs"          ~> "suspender"
    , "vaio/touchpad.hs"      ~> "vaio-touchpad"
    ]


vim = do
  role "vim" $ do
    group "haskell" $ do
      pathogen  (github "Shougo" "vimproc") $
        [sh|make -f make_unix.mak|]
      pathogen_ (github "eagletmt" "ghcmod-vim")
      pathogen_ (github "ujihisa" "neco-ghc")
      pathogen_ (github "Shougo" "neocomplcache")
      pathogen_ (github "bitc" "vim-hdevtools")
    group "coq" $ do
      pathogen_ (github "vim-scripts" "coq-syntax")
      pathogen_ (github "vim-scripts" "Coq-indent")
      pathogen_ (github "trefis" "coquille")
    group "misc" $ do
      pathogen_ (github "scrooloose" "syntastic")
      pathogen_ (github "tpope" "vim-commentary")
      pathogen_ (github "tpope" "vim-unimpaired")
      pathogen_ (github "def-lkb" "vimbufsync")
      pathogen_ (github "ivyl" "vim-bling")
      pathogen_ (github "nelstrom" "vim-visual-star-search")
      pathogen_ (github "kien" "rainbow_parentheses.vim")
      pathogen  (github "wincent" "Command-T") $
        [sh|cd ~/.vim/bundle/Command-T/ruby/command-t; /usr/bin/ruby extconf.rb; make|]
      pathogen_ (github "bling" "vim-airline")
      pathogen_ (github "stephpy" "vim-yaml")
      pathogen_ (github "roman" "golden-ratio")
    group "idris" $
      pathogen_ (github "idris-hackers" "idris-vim")
    group "rust" $
      pathogen_ (github "wting" "rust.vim")
    group "mine" $ do
      git (github "supki" "vim-flipping") (into "git") $
        register ".vim/bundle/vim-flipping"
      git (github "supki" "syntastic-cabal") (into "git") $
        register ".vim/bundle/syntastic-cabal"
      git (github "supki" "vim-languages") (into "git") $
        register ".vim/bundle/vim-languages"
      git' (github "supki" "seoul256.vim") (into ".vim/bundle") (def & branch .~ "f/m")
      pathogen_ (github "supki" "haskell-vim")
  role "vimish" $
    group "haskell" $
      pathogen_ (github "bitc" "hdevtools")
 where
  pathogen  u = git u (into ".vim/bundle")
  pathogen_ u = pathogen u (return ())


emacs = role "emacs" $ do
  group "colorschemes" $
    git (github "bbatsov" "zenburn-emacs") (into "git/emacs") $
      copyFile "zenburn-theme.el" ".emacs.d/themes/zenburn-theme.el"
  group "usable" $ do
    git (github "emacsmirror" "paredit") (into "git/emacs") $
      copyFile "paredit.el" ".emacs.d/plugins/paredit.el"
    git (github "jlr" "rainbow-delimiters") (into "git/emacs") $
      copyFile "rainbow-delimiters.el" ".emacs.d/plugins/rainbow-delimiters.el"


misc = role "misc" $ traverse_ (--> into "git")
  [ github "zsh-users" "zsh-syntax-highlighting"
  , github "zsh-users" "zsh-completions"
  , github "stepb" "urxvt-tabbedex"
  , github "muennich" "urxvt-perls"
  ]


experimental = role "experimental" $ traverse_ (--> into "git") . map (github "vimus") $
  [ "vimus"
  , "libmpd-haskell"
  ]


edwardk = role "edwardk" $ traverse_ (--> into "git") . map (github "ekmett") $
  [ "free"
  , "reflection"
  , "tagged"
  , "machines"
  , "lens"
  , "profunctors"
  , "kan-extensions"
  ]

mine = role "mine" $ do
  traverse_ (--> into "git") . map (github "supki") $
    [ "libjenkins"
    , "xmonad-screenshot"
    , "xmonad-use-empty-workspace"
    , "xmonad-2014"
    , "pakej"
    ]
  git (github "supki" "whacky") (into "git") $
    [sh|BUILDDIR=$HOME/bin make|]


infix 8 -->
(-->) :: String -> FilePath -> Script Sources ()
(-->) = git_

dot :: FilePath -> (FilePath, FilePath)
dot path = path ~> ('.' : path)

bin :: FilePath -> (FilePath, FilePath)
bin path = path ~> ("bin" </> path)

infixr 4 <\>~
(<\>~) :: Setting (->) s t FilePath FilePath -> FilePath -> s -> t
l <\>~ n = over l (n </>)


unzipWithM_ :: Applicative m => (a -> b -> m c) -> [(a, b)] -> m ()
unzipWithM_ = traverse_ . uncurry

github :: String -> String -> String
github = printf "git@github.com:%s/%s"
