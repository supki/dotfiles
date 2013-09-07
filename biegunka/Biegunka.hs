{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Lens
import Data.Default (def)
import System.FilePath ((</>))

import Control.Biegunka
import Control.Biegunka.Source.Git
import Control.Biegunka.Templates.HStringTemplate

import qualified Laptop
import qualified Work


data Environments = Laptop | Work

biegunkaOptions ''Environments


main :: IO ()
main = do
  (environment, runBiegunka) <- options
  case environment of
    Laptop -> runBiegunka (set root "~" . set templates (hStringTemplate Laptop.templates)) laptop
    Work   -> runBiegunka (set root "~" . set templates (hStringTemplate Work.templates)) work
 where
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
    ]

dotfiles, tools, vim, emacs, misc, experimental, edwardk, mine :: Script Sources ()


dotfiles = role "dotfiles" $
  git "git@github.com:supki/.dotfiles" "git/dotfiles" $ do
    cores     & mapped._1 <\>~ "core"     & unzipWithM_ link
    extendeds & mapped._1 <\>~ "extended" & unzipWithM_ link
    recipes   & mapped._1 <\>~ "extended" & unzipWithM_ substitute
    [sh|xrdb -merge ~/.Xdefaults|]
 where
  cores =
    [ "xsession"                   ~> ".xsession"
    , "mpdconf"                    ~> ".mpdconf"
    , "profile"                    ~> ".profile"
    , "bashrc"                     ~> ".bashrc"
    , "zshenv"                     ~> ".zshenv"
    , "zshrc"                      ~> ".zshrc"
    , "inputrc"                    ~> ".inputrc"
    , "vimrc"                      ~> ".vimrc"
    , "vim.custom"                 ~> ".vim/plugin/vimrc-local.vim"
    , "ghci"                       ~> ".ghci"
    , "irbrc"                      ~> ".irbrc"
    , "haskeline"                  ~> ".haskeline"
    , "racketrc"                   ~> ".racketrc"
    , "gitconfig"                  ~> ".gitconfig"
    , "gitignore"                  ~> ".gitignore"
    , "ackrc"                      ~> ".ackrc"
    , "vim/pathogen.vim"           ~> ".vim/autoload/pathogen.vim"
    , "vim/cscope_maps.vim"        ~> ".vim/bundle/cscope_maps.vim"
    , "vim/scratch"                ~> ".vim/bundle/scratch"
    , "vim/indent/haskell.vim"     ~> ".vim/indent/haskell.vim"
    , "conceal/haskell.vim"        ~> ".vim/after/syntax/haskell.vim"
    , "XCompose"                   ~> ".XCompose"
    , "vimusrc"                    ~> ".vimusrc"
    , "tmux.conf"                  ~> ".tmux.conf"
    , "emacs"                      ~> ".emacs"
    , "poneaux.rb"                 ~> ".poneaux.rb"
    , "sqliterc"                   ~> ".sqliterc"
    , "pythonrc"                   ~> ".pythonrc"
    ]
  extendeds =
    [ "xmonad.hs"                  ~> ".xmonad/xmonad.hs"
    , "xmonad/Controls.hs"         ~> ".xmonad/lib/Controls.hs"
    , "xmonad/Layouts.hs"          ~> ".xmonad/lib/Layouts.hs"
    , "xmonad/Startup.hs"          ~> ".xmonad/lib/Startup.hs"
    , "xmonad/Themes.hs"           ~> ".xmonad/lib/Themes.hs"
    , "xmonad/RouteT.hs"           ~> ".xmonad/lib/RouteT.hs"
    , "xmonad/Tmux.hs"             ~> ".xmonad/lib/Tmux.hs"
    , "xmonad/Man.hs"              ~> ".xmonad/lib/Man.hs"
    , "xmonad/Workspaces.hs"       ~> ".xmonad/lib/Workspaces.hs"
    , "gvimrc"                     ~> ".gvimrc"
    , "vimcolors"                  ~> ".vim/colors"
    , "pentadactylrc"              ~> ".pentadactylrc"
    , "pentadactyl/wanker.penta"   ~> ".pentadactyl/plugins/wanker.penta"
    , "gtkrc.mine"                 ~> ".gtkrc.mine"
    , "mplayer-config"             ~> ".mplayer/config"
    ]
  recipes =
    [ "template.xmobar.hs"         ~> ".xmobar/xmobar.hs"
    , "xmonad/Misc.hs.template"    ~> ".xmonad/lib/Misc.hs"
    , "xmonad/Profile.hs.template" ~> ".xmonad/lib/Profile.hs"
    , "xmodmap.template"           ~> ".xmodmap"
    , "Xdefaults.template"         ~> ".Xdefaults"
    ]

tools = role "tools" $
  git "git@budueba.com:tools" "git/tools" $ do
    suid_binaries & unzipWithM_ (\s t ->
      sudo "root" $ [sh|
        ghc -O2 #{s} -fforce-recomp -v0 -o #{t}
        chown root:root #{t}
        chmod +s #{t}
      |])
    user_binaries & unzipWithM_ (\s t -> do
      [sh|ghc -O2 #{s} -fforce-recomp -v0 -o #{t}|]
      link t ("bin" </> t))
    scripts & unzipWithM_ link
 where
  scripts, user_binaries, suid_binaries :: [(String, String)]
  scripts =
    [ "youtube-in-mplayer.sh" ~> "bin/youtube-in-mplayer"
    , "cue2tracks.sh"         ~> "bin/cue2tracks"
    , "weather.rb"            ~> "bin/ask-weather"
    , "playcount.hs"          ~> "bin/playcount"
    , "mpd/.lastfm.conf"      ~> ".lastfm.conf"
    , "mpd/lastfm.png"        ~> ".icons/lastfm.png"
    , "mpd/love.hs"           ~> "bin/lastfm-love-current-mpd-track"
    , "battery.rb"            ~> "bin/vaio-battery"
    , "pemised.rb"            ~> "bin/pemised"
    , "upload/screenshot.sh"  ~> "bin/upload-screenshot"
    , "upload/budueba.sh"     ~> "bin/upload-budueba"
    , "upload/pastebin.hs"    ~> "bin/upload-pastebin"
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
    ]
  suid_binaries =
    [ "suspender.hs"          ~> "suspender"
    , "vaio/touchpad.hs"      ~> "vaio-touchpad"
    ]


vim = do
  role "vim" $ do
    group "haskell" $ do
      pathogen  "git@github.com:Shougo/vimproc" $
        [sh|make -f make_unix.mak|]
      pathogen_ "git@github.com:eagletmt/ghcmod-vim"
      pathogen_ "git@github.com:ujihisa/neco-ghc"
      pathogen_ "git@github.com:Shougo/neocomplcache"
      pathogen_ "git@github.com:bitc/vim-hdevtools"
      pathogen_ "git@github.com:merijn/haskellFoldIndent"
    group "ruby" $ do
      pathogen_ "git@github.com:kana/vim-textobj-user"
      pathogen_ "git@github.com:nelstrom/vim-textobj-rubyblock"
      pathogen_ "git@github.com:rking/vim-detailed"
    group "coq" $ do
      pathogen_ "git@github.com:vim-scripts/coq-syntax"
      pathogen_ "git@github.com:vim-scripts/Coq-indent"
      pathogen_ "git@github.com:trefis/coquille"
    group "misc" $ do
      pathogen_ "git@github.com:wikitopian/hardmode"
      pathogen_ "git@github.com:scrooloose/syntastic"
      pathogen_ "git@github.com:Shougo/unite.vim"
      pathogen_ "git@github.com:spolu/dwm.vim"
      pathogen_ "git@github.com:tpope/vim-commentary"
      pathogen_ "git@github.com:tpope/vim-unimpaired"
      pathogen_ "git@github.com:def-lkb/vimbufsync"
    group "agda" $
      pathogen_ "git@github.com:supki/agda-vim"
    group "idris" $
      "git@github.com:edwinb/Idris-dev" ==> into "git" $ def
        & remotes .~ ["origin", "stream"]
        & actions .~
            link "contribs/tool-support/vim" ".vim/bundle/idris-vim"
    group "text" $
      pathogen_ "git@github.com:godlygeek/tabular"
  role "vimish" $
    group "haskell" $
      pathogen_ "git@github.com:bitc/hdevtools"
 where
  pathogen  u = git u (into ".vim/bundle")
  pathogen_ u = pathogen u (return ())


emacs = role "emacs" $ do
  group "colorschemes" $
    git "git@github.com:bbatsov/zenburn-emacs" (into "git/emacs") $
      copyFile "zenburn-theme.el" ".emacs.d/themes/zenburn-theme.el"
  group "usable" $ do
    git "git@github.com:emacsmirror/paredit" (into "git/emacs") $
      copyFile "paredit.el" ".emacs.d/plugins/paredit.el"
    git "git@github.com:jlr/rainbow-delimiters" (into "git/emacs") $
      copyFile "rainbow-delimiters.el" ".emacs.d/plugins/rainbow-delimiters.el"


misc = role "misc" $ mapM_ (--> into "git")
  [ "git@github.com:zsh-users/zsh-syntax-highlighting"
  , "git@github.com:zsh-users/zsh-completions"
  , "git@github.com:stepb/urxvt-tabbedex"
  , "git@github.com:dmalikov/xmobar-usable"
  , "git@github.com:muennich/urxvt-perls"
  ]


experimental = role "experimental" $ mapM_ (--> into "git")
  [ "git@github.com:sol/vimus"
  , "git@github.com:sol/libmpd-haskell"
  , "git@github.com:mitchellh/vagrant"
  ]


edwardk = role "edwardk" $ mapM_ (--> into "git")
  [ "git@github.com:ekmett/free"
  , "git@github.com:ekmett/reflection"
  , "git@github.com:ekmett/tagged"
  , "git@github.com:ekmett/machines"
  , "git@github.com:ekmett/lens"
  , "git@github.com:ekmett/profunctors"
  , "git@github.com:ekmett/kan-extensions"
  ]

mine = role "mine" $ mapM_ (--> into "git")
  [ "git@github.com:supki/libjenkins"
  ]


infix 8 -->
(-->) :: String -> FilePath -> Script Sources ()
(-->) = git_

infix 4 ~>
(~>) :: a -> b -> (a, b)
(~>) = (,)

infixr 4 <\>~
(<\>~) :: Setting (->) s t FilePath FilePath -> FilePath -> s -> t
l <\>~ n = over l (n </>)


unzipWithM_ :: Monad m => (a -> b -> m c) -> [(a, b)] -> m ()
unzipWithM_ = mapM_ . uncurry
