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
  , my float     --> doFloat
  , my ignore    --> doIgnore
  , my relax     --> doShift (toWsId Talkative)
  , my chromie   --> doShift (toWsId WWW)
  , my docs      --> doShift (toWsId Texts)
  , my video     --> doShift (toWsId Video)
  , my status    --> doShift (toWsId Status)
  , my mail      --> doShift (toWsId Mail)
  , my files     --> doShift (toWsId Files)
  , my torrent   --> doShift (toWsId Torrents)
  , my stuff     --> doShift (toWsId Stuff)
  ]
  <> manageDocks <> manageHook defaultConfig
 where
  float =
    [ title     <&> ("Figure" `isPrefixOf`)
    , title     =? "youtube-video"
    , title     =? "xmessage"
    ]
  ignore =
    [ title     =? "xfce4-notifyd"
    ]
  status =
    [ title     <&> ("htop" `isInfixOf`)
    , title     =? "iotop"
    , title     =? "netstat"
    , title     <&> ("vimus" `isPrefixOf`)
    , title     =? "poneaux"
    ]
  chromie =
    [ className =? "Chromium-browser"
    , className =? "Chromium"
    , className =? "Iceweasel"
    , className =? "Firefox"
    ]
  relax =
    [ title     =? "mcabber"
    , title     =? "irssi"
    , title     =? "youtube-video"
    ]
  docs =
    [ className =? "OpenOffice.org 3.2"
    , className =? "Apvlv"
    , className =? "Xchm"
    , className =? "Evince"
    ]
  video =
    [ className =? "Vlc"
    , className =? "MPlayer"
    , className =? "mplayer2"
    ]
  mail =
    [ className =? "Thunderbird"
    , className =? "Icedove"
    ]
  files =
    [ className =? "Gnome-commander"
    , title     =? "mc"
    ]
  torrent =
    [ title     =? "rtorrent"
    , title     =? "Transmission"
    , title     =? "Torrent Options"
    ]
  stuff =
    [ title     <&> ("stuff" `isSuffixOf`)
    , title     =? "CuteCom"
    ]

  my = foldr1 (<||>)
--

-- Scratchpads
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "scratchpad" (myTerminal ++ " -name scratchpad") (resource =? "scratchpad") (customFloating $ W.RationalRect 0.47 0.05 0.5 0.6)
  ]
