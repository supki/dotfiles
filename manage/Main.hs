{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Control.Lens
import Data.Default (def)
import Options.Applicative hiding ((&))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.FilePath.Lens

import Biegunka
import Biegunka.Source.Git

import Templates (laptopTemplates, workTemplates)


main ∷ IO ()
main = execParser opts >>= \(s,t) → do
  home <- getHomeDirectory
  biegunka (def & root .~ home) s $
    pretend <> pause <> execute (def & templates .~ Templates t & parallel .~ True) <> verify
 where
  opts = info (helper <*> sample) (fullDesc <> header "Biegunka script")

  sample =
     flag (return (), def) (laptopSettings, laptopTemplates) (long "laptop" <> short 'l' <> help "Use laptop settings") <|>
     flag (return (), def) (workSettings, workTemplates) (long "work" <> short 'w' <> help "Use work settings")

  laptopSettings = sequence_ [dotfiles, tools, vim, misc, experimental, edwardk]
  workSettings = sequence_ [dotfiles, vim, misc]


dotfiles ∷ Script Profiles
dotfiles = profile "dotfiles" $
  git "git@github.com:supki/.dotfiles" "git/dotfiles" $ do
    mapM_ (uncurry link) $ mapped . _1 <\>~ "core" $
      [ ("xsession", ".xsession")
      , ("mpdconf", ".mpdconf")
      , ("profile", ".profile")
      , ("bashrc", ".bashrc")
      , ("zshenv", ".zshenv")
      , ("zshrc", ".zshrc")
      , ("inputrc", ".inputrc")
      , ("vimrc", ".vimrc")
      , ("ghci", ".ghci")
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
      ]
    mapM_ (uncurry link) $ mapped . _1 <\>~ "extended" $
      [ ("xmonad.hs", ".xmonad/xmonad.hs")
      , ("xmonad/Controls.hs", ".xmonad/lib/Controls.hs")
      , ("xmonad/Layouts.hs", ".xmonad/lib/Layouts.hs")
      , ("xmonad/Startup.hs", ".xmonad/lib/Startup.hs")
      , ("xmonad/Themes.hs", ".xmonad/lib/Themes.hs")
      , ("xmonad/Tmux.hs", ".xmonad/lib/Tmux.hs")
      , ("xmonad/Workspaces.hs", ".xmonad/lib/Workspaces.hs")
      , ("gvimrc", ".gvimrc")
      , ("vimcolors", ".vim/colors")
      , ("pentadactylrc", ".pentadactylrc")
      , ("pentadactyl/wanker.penta", ".pentadactyl/plugins/wanker.penta")
      , ("gtkrc.mine", ".gtkrc.mine")
      , ("xmobar.hs", ".xmobar/xmobar.hs")
      ]
    mapM_ (uncurry substitute) $ mapped . _1 <\>~ "extended" $
      [ ("xmobarrc.template", ".xmobarrc")
      , ("xmonad/Misc.hs.template", ".xmonad/lib/Misc.hs")
      , ("xmonad/Profile.hs.template", ".xmonad/lib/Profile.hs")
      , ("xmodmap.template", ".xmodmap")
      , ("Xdefaults.template", ".Xdefaults")
      ]


tools ∷ Script Profiles
tools = profile "tools" $
  git "git@budueba.com:tools" "git/tools" $ do
    mapM_ (uncurry link)
      [ ("youtube-in-mplayer.sh", "bin/youtube-in-mplayer")
      , ("cue2tracks.sh", "bin/cue2tracks")
      , ("weather.rb", "bin/ask-weather")
      , ("mpd/.lastfm.conf", ".lastfm.conf")
      , ("mpd/lastfm.png", ".icons/lastfm.png")
      , ("mpd/love.hs", "bin/lastfm-love-current-mpd-track")
      , ("trayicon/mcabber.py", "bin/trayicon-mcabber")
      , ("trayicon/icons/mcabber-default.png", ".icons/mcabber-default.png")
      , ("trayicon/icons/mcabber-unread.png", ".icons/mcabber-unread.png")
      , ("trayicon/mpd.py", "bin/trayicon-mpd")
      , ("trayicon/icons/mpd-pause.png", ".icons/mpd-pause.png")
      , ("trayicon/icons/mpd-playing.png", ".icons/mpd-playing.png")
      , ("battery.rb", "bin/vaio-battery")
      , ("upload/screenshot.sh", "bin/upload-screenshot")
      , ("upload/budueba.sh", "bin/upload-budueba")
      , ("upload/pastebin.hs", "bin/upload-pastebin")
      , ("isup.sh", "bin/isup")
      , ("pretty-json.py", "bin/pretty-json")
      ]
    mapM_ (uncurry (\s d → shell ("ghc -O2 " ++ s ++ " -fforce-recomp -v0 -o " ++ d) >> link d ("bin" </> d)))
      [ ("mpd/scrobbler.hs", "liblastfm-scrobbler")
      , ("audio.hs", "vaio-audio")
      , ("shutdown-gui.hs", "shutdown-gui")
      ]


vim ∷ Script Profiles
vim = do
  profile "vim-haskell" $ do
    pathogen  "git@github.com:Shougo/vimproc" $
      shell "make -f make_unix.mak"
    pathogen_ "git@github.com:eagletmt/ghcmod-vim"
    pathogen_ "git@github.com:ujihisa/neco-ghc"
    pathogen_ "git@github.com:Shougo/neocomplcache"
  profile "vim-coq" $ do
    pathogen_ "git@github.com:vim-scripts/coq-syntax"
    pathogen_ "git@github.com:vim-scripts/Coq-indent"
  profile "vim-misc" $ do
    pathogen_ "git@github.com:scrooloose/syntastic.git"
    pathogen_ "git@github.com:Shougo/unite.vim"
    pathogen_ "git@github.com:spolu/dwm.vim"
    pathogen_ "git@github.com:tpope/vim-commentary"
 where
  pathogen  u = git  u (".vim/bundle" </> u ^. basename)
  pathogen_ u = pathogen u (return ())


misc ∷ Script Profiles
misc = profile "misc" $ do
  "git@github.com:zsh-users/zsh-completions.git" --> "git/zsh-completions"
  "git@github.com:stepb/urxvt-tabbedex"          --> "git/urxvt-tabbedex"


experimental ∷ Script Profiles
experimental = profile "experimental" $ do
  "https://github.com/sol/vimus"          --> "git/vimus"
  "https://github.com/sol/libmpd-haskell" --> "git/libmpd-haskell"


edwardk :: Script Profiles
edwardk = profile "edwardk" $ do
  "git@github.com:ekmett/free"        --> "git/free"
  "git@github.com:ekmett/reflection"  --> "git/reflection"
  "git@github.com:ekmett/tagged"      --> "git/tagged"
  "git@github.com:ekmett/machines"    --> "git/machines"
  "git@github.com:ekmett/lens"        --> "git/lens"
  "git@github.com:ekmett/profunctors" --> "git/profunctors"


infix 1 -->
(-->) ∷ String → FilePath → Script Sources
(-->) = git_


infixr 4 <\>~
(<\>~) :: Setting (->) s t FilePath FilePath -> FilePath -> s -> t
l <\>~ n = over l (n </>)
