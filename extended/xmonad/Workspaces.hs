module Workspaces where

import Data.Functor ((<$>))
import Data.List (isPrefixOf)
import Data.Word (Word64)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.PerWorkspace as XLP
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
data Workspace = Talkative | WWW | Texts | Video | Status | Mail | Files | Torrents

toWsId :: Workspace -> WorkspaceId
toWsId Talkative = "~"
toWsId WWW = "1"
toWsId Texts = "8"
toWsId Video = "9"
toWsId Status = "0"
toWsId Mail = "-"
toWsId Files = "="
toWsId Torrents = "\\"

onWorkspace ws = XLP.onWorkspace (toWsId ws)
--

-- Manage hook
myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook scratchpads <+> composeAll
  [ isFullscreen --> doFullFloat
  , myFloat      --> doFloat
  , myRelax      --> doShift (toWsId Talkative)
  , myChromie    --> doShift (toWsId WWW)
  , myDocs       --> doShift (toWsId Texts)
  , myVideo      --> doShift (toWsId Video)
  , myStatus     --> doShift (toWsId Status)
  , myMail       --> doShift (toWsId Mail)
  , myFiles      --> doShift (toWsId Files)
  , myTorrent    --> doShift (toWsId Torrents)
  ]
  <+> manageDocks <+> manageHook defaultConfig
  where
  myFloat = foldr1 (<||>)
    [ ("Figure" `isPrefixOf`) <$> title
    , title     =? "youtube-video"
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
  [ NS "scratchpad" "urxvtc -name scratchpad" (resource =? "scratchpad") (customFloating $ W.RationalRect 0.25 0.20 0.5 0.6)
  , NS "ncmpcpp" "urxvtc -name ncmpcpp -e ncmpcpp" (resource =? "ncmpcpp") (customFloating $ W.RationalRect 0.25 0.20 0.5 0.6)
  ]
