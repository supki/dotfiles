{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Lens
import Data.Default (def)
import System.FilePath ((</>))

import Biegunka
import Biegunka.Source.Git

import qualified Laptop as Laptop
import qualified Work as Work


data Environments = Laptop | Work

makeOptionsParser ''Environments


main :: IO ()
main = do
  (environment, runBiegunka) <- optionsParser
  case environment of
    Laptop -> runBiegunka (set root "~") (set templates (Templates Laptop.templates)) laptop
    Work   -> runBiegunka (set root "~") (set templates (Templates Work.templates)) work
 where
  laptop = sequence_
    [ dotfiles
    , tools
    , vim
    , emacs
    , misc
    , experimental
    , edwardk
    ]
  work = sequence_
    [ dotfiles
    , vim
    , misc
    , experimental
    ]

dotfiles, tools, vim, emacs, misc, experimental, edwardk :: Script Profiles ()


dotfiles = profile "dotfiles" $
  git "git@github.com:supki/.dotfiles" "git/dotfiles" $ do
    unzipWithM_ link $
      [ ("xsession", ".xsession")
      , ("mpdconf", ".mpdconf")
      , ("profile", ".profile")
      , ("bashrc", ".bashrc")
      , ("zshenv", ".zshenv")
      , ("zshrc", ".zshrc")
      , ("inputrc", ".inputrc")
      , ("vimrc", ".vimrc")
      , ("vim.custom", ".vim/plugin/vimrc-local.vim")
      , ("ghci", ".ghci")
      , ("irbrc", ".irbrc")
      , ("haskeline", ".haskeline")
      , ("racketrc", ".racketrc")
      , ("gitconfig", ".gitconfig")
      , ("gitignore", ".gitignore")
      , ("ackrc", ".ackrc")
      , ("vim/pathogen.vim", ".vim/autoload/pathogen.vim")
      , ("vim/cscope_maps.vim", ".vim/bundle/cscope_maps.vim")
      , ("vim/scratch", ".vim/bundle/scratch")
      , ("vim/indent/haskell.vim", ".vim/indent/haskell.vim")
      , ("conceal/haskell.vim", ".vim/after/syntax/haskell.vim")
      , ("XCompose", ".XCompose")
      , ("vimusrc", ".vimusrc")
      , ("tmux.conf", ".tmux.conf")
      , ("emacs", ".emacs")
      , ("poneaux.rb", ".poneaux.rb")
      , ("sqliterc", ".sqliterc")
      ] & mapped . _1 <\>~ "core"
    unzipWithM_ link $
      [ ("xmonad.hs", ".xmonad/xmonad.hs")
      , ("xmonad/Controls.hs", ".xmonad/lib/Controls.hs")
      , ("xmonad/Layouts.hs", ".xmonad/lib/Layouts.hs")
      , ("xmonad/Startup.hs", ".xmonad/lib/Startup.hs")
      , ("xmonad/Themes.hs", ".xmonad/lib/Themes.hs")
      , ("xmonad/Tmux.hs", ".xmonad/lib/Tmux.hs")
      , ("xmonad/Man.hs", ".xmonad/lib/Man.hs")
      , ("xmonad/Workspaces.hs", ".xmonad/lib/Workspaces.hs")
      , ("gvimrc", ".gvimrc")
      , ("vimcolors", ".vim/colors")
      , ("pentadactylrc", ".pentadactylrc")
      , ("pentadactyl/wanker.penta", ".pentadactyl/plugins/wanker.penta")
      , ("gtkrc.mine", ".gtkrc.mine")
      , ("mplayer-config", ".mplayer/config")
      ] & mapped . _1 <\>~ "extended"
    unzipWithM_ substitute $
      [ ("template.xmobar.hs", ".xmobar/xmobar.hs")
      , ("xmonad/Misc.hs.template", ".xmonad/lib/Misc.hs")
      , ("xmonad/Profile.hs.template", ".xmonad/lib/Profile.hs")
      , ("xmodmap.template", ".xmodmap")
      , ("Xdefaults.template", ".Xdefaults")
      ] & mapped . _1 <\>~ "extended"
    shell "xrdb -merge ~/.Xdefaults"


tools = profile "tools" $
  git "git@budueba.com:tools" "git/tools" $ do
    unzipWithM_ link
      [ ("youtube-in-mplayer.sh", "bin/youtube-in-mplayer")
      , ("cue2tracks.sh", "bin/cue2tracks")
      , ("weather.rb", "bin/ask-weather")
      , ("playcount.hs", "bin/playcount")
      , ("mpd/.lastfm.conf", ".lastfm.conf")
      , ("mpd/lastfm.png", ".icons/lastfm.png")
      , ("mpd/love.hs", "bin/lastfm-love-current-mpd-track")
      , ("battery.rb", "bin/vaio-battery")
      , ("pemised.rb", "bin/pemised")
      , ("upload/screenshot.sh", "bin/upload-screenshot")
      , ("upload/budueba.sh", "bin/upload-budueba")
      , ("upload/pastebin.hs", "bin/upload-pastebin")
      , ("isup.sh", "bin/isup")
      , ("pretty-json.py", "bin/pretty-json")
      , ("publish-haddocks.sh", "bin/publish-haddocks")
      ]
    unzipWithM_ (\s d -> command "ghc" ["-O2 ", s, "-fforce-recomp", "-v0", "-o", d] >> link d ("bin" </> d))
      [ ("audio.hs", "vaio-audio")
      , ("shutdown-gui.hs", "shutdown-gui")
      ]


vim = do
  profile "vim/haskell" $ do
    pathogen  "git@github.com:Shougo/vimproc" $
      shell "make -f make_unix.mak"
    pathogen_ "git@github.com:eagletmt/ghcmod-vim"
    pathogen_ "git@github.com:ujihisa/neco-ghc"
    pathogen_ "git@github.com:Shougo/neocomplcache"
    pathogen_ "git@github.com:bitc/vim-hdevtools"
    pathogen_ "git@github.com:merijn/haskellFoldIndent"
  profile "vim/ruby" $ do
    pathogen_ "git@github.com:kana/vim-textobj-user"
    pathogen_ "git@github.com:nelstrom/vim-textobj-rubyblock"
  profile "vimish/haskell" $ do
    pathogen_ "git@github.com:bitc/hdevtools"
  profile "vim/coq" $ do
    pathogen_ "git@github.com:vim-scripts/coq-syntax"
    pathogen_ "git@github.com:vim-scripts/Coq-indent"
    pathogen_ "git@github.com:trefis/coquille"
  profile "vim/misc" $ do
    pathogen_ "git@github.com:wikitopian/hardmode"
    pathogen_ "git@github.com:scrooloose/syntastic"
    pathogen_ "git@github.com:Shougo/unite.vim"
    pathogen_ "git@github.com:spolu/dwm.vim"
    pathogen_ "git@github.com:tpope/vim-commentary"
    pathogen_ "git@github.com:tpope/vim-unimpaired"
    pathogen_ "git@github.com:def-lkb/vimbufsync"
  profile "vim/idris" $ do
    "git@github.com:edwinb/Idris-dev" ==> "git/" $ def
      & remotes .~ ["origin", "stream"]
      & actions .~ do
          link "contribs/tool-support/vim" ".vim/bundle/idris-vim"
 where
  pathogen  u = git u ".vim/bundle/"
  pathogen_ u = pathogen u (return ())


emacs = do
  profile "emacs-colorschemes" $ do
    git "git@github.com:bbatsov/zenburn-emacs" "git/emacs/" $
      copy "zenburn-theme.el" ".emacs.d/themes/zenburn-theme.el"
  profile "emacs-usable" $ do
    git "git@github.com:emacsmirror/paredit" "git/emacs/" $
      copy "paredit.el" ".emacs.d/plugins/paredit.el"
    git "git@github.com:jlr/rainbow-delimiters" "git/emacs/" $
      copy "rainbow-delimiters.el" ".emacs.d/plugins/rainbow-delimiters.el"



misc = profile "misc" $ mapM_ (--> "git/")
  [ "git@github.com:zsh-users/zsh-syntax-highlighting"
  , "git@github.com:zsh-users/zsh-completions"
  , "git@github.com:stepb/urxvt-tabbedex"
  , "git@github.com:dmalikov/xmobar-usable"
  , "git@github.com:muennich/urxvt-perls"
  ]


experimental = profile "experimental" $ mapM_ (--> "git/")
  [ "git@github.com:sol/vimus"
  , "git@github.com:sol/libmpd-haskell"
  , "git@github.com:mitchellh/vagrant"
  , "git@github.com:haskell/cabal"
  ]


edwardk = profile "edwardk" $ mapM_ (--> "git/")
  [ "git@github.com:ekmett/free"
  , "git@github.com:ekmett/reflection"
  , "git@github.com:ekmett/tagged"
  , "git@github.com:ekmett/machines"
  , "git@github.com:ekmett/lens"
  , "git@github.com:ekmett/profunctors"
  ]


infix 8 -->
(-->) :: String -> FilePath -> Script Sources ()
(-->) = git_

infixr 4 <\>~
(<\>~) :: Setting (->) s t FilePath FilePath -> FilePath -> s -> t
l <\>~ n = over l (n </>)


unzipWithM_ :: Monad m => (a -> b -> m c) -> [(a, b)] -> m ()
unzipWithM_ = mapM_ . uncurry
