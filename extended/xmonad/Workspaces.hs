module Workspaces where

import Control.Lens
import Data.Functor ((<$>))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Monoid ((<>), mconcat)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.PerWorkspace as XLPW
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import Misc

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = concat
  [ ["~"]
  , map show [1..7]
  , ["8", "9", "0", "-", "=", "\\", "<-"]
  ]

myWorkspacesKeys :: [KeySym]
myWorkspacesKeys = concat
  [ [xK_grave]
  , [xK_1..xK_7]
  , [xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_backslash, xK_BackSpace]
  ]
--

-- Ids
data Workspace =
    Talkative
  | WWW
  | Texts
  | Video
  | Status
  | Mail
  | Files
  | Torrents
  | Stuff

toWsId :: Workspace -> WorkspaceId
toWsId Talkative = "~"
toWsId WWW = "1"
toWsId Texts = "8"
toWsId Video = "9"
toWsId Status = "0"
toWsId Mail = "-"
toWsId Files = "="
toWsId Torrents = "\\"
toWsId Stuff = "<-"

onWorkspace w = XLPW.onWorkspace (toWsId w)
onWorkspaces ws = XLPW.onWorkspaces (map toWsId ws)
--

-- Manage hook
myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook scratchpads <> mconcat
  [ isFullscreen --> doFullFloat
  , myFloat      --> doFloat
  , myIgnore     --> doIgnore
  , myRelax      --> doShift (toWsId Talkative)
  , myChromie    --> doShift (toWsId WWW)
  , myDocs       --> doShift (toWsId Texts)
  , myVideo      --> doShift (toWsId Video)
  , myStatus     --> doShift (toWsId Status)
  , myMail       --> doShift (toWsId Mail)
  , myFiles      --> doShift (toWsId Files)
  , myTorrent    --> doShift (toWsId Torrents)
  , myStuff      --> doShift (toWsId Stuff)
  ]
  <> manageDocks <> manageHook defaultConfig
  where
  myFloat = foldr1 (<||>)
    [ title <&> ("Figure" `isPrefixOf`)
    , title     =? "youtube-video"
    , title     =? "xmessage"
    ]
  myIgnore = foldr1 (<||>)
    [ title     =? "xfce4-notifyd"
    ]
  myStatus = foldr1 (<||>)
    [ title <&> ("htop" `isInfixOf`)
    , title     =? "iotop"
    , title     =? "netstat"
    , title     =? "poneaux"
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
    [ title     =? "rtorrent"
    , title     =? "Transmission"
    , title     =? "Torrent Options"
    ]
  myStuff = foldr1 (<||>)
    [ title <&> ("stuff" `isSuffixOf`)
    ]
--

-- Scratchpads
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "scratchpad" (myTerminal ++ " -name scratchpad") (resource =? "scratchpad") (customFloating $ W.RationalRect 0.47 0.05 0.5 0.6)
  , NS "vimus" (myTerminal ++ " -name vimus -e zsh -c vimus") (resource =? "vimus") (customFloating $ W.RationalRect 0.25 0.20 0.5 0.6)
  ]
