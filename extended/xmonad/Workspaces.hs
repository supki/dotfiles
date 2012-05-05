module Workspaces where

import Data.Functor ((<$>))
import Data.List (isPrefixOf)
import Data.Word (Word64)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import Themes

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = concat
  [ ["~"]
  , map show [1..7]
  , map ((++ "'") . show) [1..8]
  , ["8", "9", "0", "-", "=", "\\", "<-"]
  ]

myWorkspacesKeys :: [Word64]
myWorkspacesKeys = concat
  [ [xK_grave]
  , [xK_1..xK_7]
  , [xK_F1..xK_F8]
  , [xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_backslash, xK_BackSpace]
  ]
--

-- Ids
toId :: Int -> WorkspaceId
toId = (!!) myWorkspaces

relax = 0
chromie = 1
docs = 8
video = 9
status = 10
mail = 11
files = 12
torrent = 13
--

-- Manage hook
myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook scratchpads <+> composeAll
    -- automatically switching app to workspace
    [ isFullscreen --> doFullFloat
    , myFloat      --> doFloat
    , myRelax      --> (doShift . toId) relax
    , myChromie    --> (doShift . toId) chromie
    , myDocs       --> (doShift . toId) docs
    , myVideo      --> (doShift . toId) video
    , myStatus     --> (doShift . toId) status
    , myMail       --> (doShift . toId) mail
    , myFiles      --> (doShift . toId) files
    , myTorrent    --> (doShift . toId) torrent
    ]
    <+> manageDocks <+> manageHook defaultConfig
  where
  myFloat = foldr1 (<||>)
    [ ("Figure" `isPrefixOf`) <$> title
    ]
  myStatus = foldr1 (<||>)
    [ title     =? "htop"
    , title     =? "iotop"
    , title     =? "netstat"
    ]
  myChromie = foldr1 (<||>)
    [ className =? "Chromium-browser"
    , className =? "Chromium"
    , className =? "Iceweasel"
    , className =? "Firefox"
    ]
  myRelax = foldr1 (<||>)
    [ title     =? "mcabber"
    , title     =? "irssi"
    , title     =? "youtube-video"
    ]
  myDocs = foldr1 (<||>)
    [ className =? "OpenOffice.org 3.2"
    , className =? "Apvlv"
    , className =? "Xchm"
    , className =? "Evince"
    ]
  myVideo = foldr1 (<||>)
    [ className =? "Vlc"
    , className =? "MPlayer"
    , className =? "mplayer2"
    ]
  myMail = foldr1 (<||>)
    [ className =? "Thunderbird"
    , className =? "Icedove"
    ]
  myFiles = foldr1 (<||>)
    [ className =? "Gnome-commander"
    , title     =? "mc"
    ]
  myTorrent = foldr1 (<||>)
    [ title =? "rtorrent"
    , title =? "Transmission"
    ]
--

-- Scratchpads
scratchpads =
  [ NS "scratchpad" "urxvtc -name scratchpad" (resource =? "scratchpad") (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5)
  , NS "ncmpcpp" "urxvtc -name ncmpcpp -e ncmpcpp" (resource =? "ncmpcpp") (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5)
  ]
