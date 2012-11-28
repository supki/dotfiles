{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Data.Monoid ((<>))

import Control.Lens
import Data.Default (def)
import Options.Applicative hiding ((&))
import System.FilePath ((</>))

import Biegunka
import Biegunka.Source.Git

import Templates (laptopTemplates, workTemplates)


main ∷ IO ()
main = execParser opts >>= \(s,t) → s &
  pretend >-> executeWith (defaultExecution & templates .~ t) >-> verify
 where
  opts = info (helper <*> sample) (fullDesc <> header "Biegunka script")

  sample =
     flag (return (), def) (laptopSettings, laptopTemplates) (long "laptop" <> short 'l' <> help "Use laptop settings") <|>
     flag (return (), def) (workSettings, workTemplates) (long "work" <> short 'w' <> help "Use work settings")

  (>->) = liftA2 (>>)

  laptopSettings = sequence_ [dotfiles, tools, vim, misc, experimental]
  workSettings = sequence_ [dotfiles, vim, misc]


misc ∷ Script Profile ()
misc = profile "misc" $ do
  "git@github.com:zsh-users/zsh-completions.git" --> "git/zsh-completions"
  "git@github.com:stepb/urxvt-tabbedex"          --> "git/urxvt-tabbedex"
  "git@github.com:supki/zsh-cabal-completion"    --> "git/zsh-cabal-completion"


experimental ∷ Script Profile ()
experimental = profile "experimental" $ do
  "https://github.com/sol/vimus"          --> "git/vimus"
  "https://github.com/sol/libmpd-haskell" --> "git/libmpd-haskell"


dotfiles ∷ Script Profile ()
dotfiles = profile "dotfiles" $
  git "git@github.com:supki/.dotfiles" "git/dotfiles" $ do
    ex link $ traverse . _1 %~ ("core" </>) $
      [ ("xsession", ".xsession")
      , ("mpdconf", ".mpdconf")
      , ("bashrc", ".bashrc")
      , ("zshrc", ".zshrc")
      , ("inputrc", ".inputrc")
      , ("profile", ".profile")
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
      ]
    ex link $ traverse . _1 %~ ("extended" </>) $
      [ ("xmonad.hs", ".xmonad/xmonad.hs")
      , ("xmonad/Controls.hs", ".xmonad/lib/Controls.hs")
      , ("xmonad/Layouts.hs", ".xmonad/lib/Layouts.hs")
      , ("xmonad/Startup.hs", ".xmonad/lib/Startup.hs")
      , ("xmonad/Themes.hs", ".xmonad/lib/Themes.hs")
      , ("xmonad/Workspaces.hs", ".xmonad/lib/Workspaces.hs")
      , ("gvimrc", ".gvimrc")
      , ("vimcolors", ".vim/colors")
      , ("pentadactylrc", ".pentadactylrc")
      , ("pentadactyl/wanker.penta", ".pentadactyl/plugins/wanker.penta")
      , ("gtkrc.mine", ".gtkrc.mine")
      , ("xmobar.hs", ".xmobar/xmobar.hs")
      ]
    ex substitute $ traverse . _1 %~ ("extended" </>) $
      [ ("xmobarrc.template", ".xmobarrc")
      , ("xmonad/Misc.hs.template", ".xmonad/lib/Misc.hs")
      , ("xmonad/Profile.hs.template", ".xmonad/lib/Profile.hs")
      , ("xmodmap.template", ".xmodmap")
      , ("Xdefaults.template", ".Xdefaults")
      ]


tools ∷ Script Profile ()
tools = profile "tools" $
  git "git@budueba.com:tools" "git/tools" $ do
    ex link
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
      ]
    ex (\s d → shell ("ghc -O2 " ++ s ++ " -fforce-recomp -v0 -o " ++ d) >> link d ("bin" </> d))
      [ ("mpd/scrobbler.hs", "liblastfm-scrobbler")
      , ("audio.hs", "vaio-audio")
      , ("shutdown-gui.hs", "shutdown-gui")
      ]


vim ∷ Script Profile ()
vim = profile "vim" $ do
  "git@github.com:Shougo/vimproc"               -->/ ".vim/bundle/vimproc" $
    shell "make -f make_unix.mak"
  "git@github.com:eagletmt/ghcmod-vim"          --> ".vim/bundle/ghcmod-vim"
  "git@github.com:ujihisa/neco-ghc"             --> ".vim/bundle/neco-ghc"
  "git@github.com:Shougo/neocomplcache"         --> ".vim/bundle/neocomplcache"
  "git@github.com:Lokaltog/vim-easymotion"      --> ".vim/bundle/easymotion"

  "git@github.com:vim-scripts/YankRing.vim"     --> ".vim/bundle/YankRing"
  "git@github.com:Shougo/unite.vim"             --> ".vim/bundle/unite"
  "git@github.com:spolu/dwm.vim"                --> ".vim/bundle/dwm"
  "git@github.com:vim-scripts/bufexplorer.zip"  --> ".vim/bundle/be"
  "git@github.com:rosstimson/scala-vim-support" --> ".vim/bundle/scala-vim-support"


ex ∷ Monad m ⇒ (FilePath → FilePath → m a) → [(FilePath, FilePath)] → m ()
ex = mapM_ . uncurry


(-->) ∷ String → FilePath → Script Source ()
(-->) = git_


(-->/) ∷ String → FilePath → Script Files () → Script Source ()
(-->/) = git
