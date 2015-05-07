{-# LANGUAGE DeriveDataTypeable #-}
module Defaults
  ( Template
  , Xmobar(..)
  , Xmodmap(..)
  , Xmonad(..)
  , Xsession(..)
  , Urxvt(..)
  , template
  ) where

import Data.Data (Data, Typeable)


data Template = Template
  { xmobar   :: Xmobar
  , xmonad   :: Xmonad
  , xmodmap  :: Xmodmap
  , xsession :: Xsession
  , urxvt    :: Urxvt
  } deriving (Data, Typeable)

type Mod a = a -> a

template :: Mod Xmobar -> Mod Xmonad -> Mod Xmodmap -> Mod Xsession -> Mod Urxvt -> Template
template f g h j k = Template
  { xmobar   = f defaultXmobar
  , xmonad   = g defaultXmonad
  , xmodmap  = h defaultXmodmap
  , xsession = j defaultXsession
  , urxvt    = k defaultUrxvt
  }

data Xmobar = Xmobar
  { background, position :: String
  , battery              :: Maybe String
  } deriving (Data, Typeable)

defaultXmobar :: Xmobar
defaultXmobar = Xmobar
  { background = ""
  , position   = ""
  , battery    = Just "\"\""
  }

data Xmonad = Xmonad
  { terminal
  , ubuntu
  , terminus
  , white
  , darkGray
  , lightGray
  , black
  , blue
  , orange
  , yellow
  , startup
  , follow
  , patterns :: String
  } deriving (Data, Typeable)

defaultXmonad :: Xmonad
defaultXmonad = Xmonad
  { terminal  = ""
  , ubuntu    = ""
  , terminus  = ""
  , white     = ""
  , darkGray  = ""
  , lightGray = ""
  , black     = ""
  , blue      = ""
  , orange    = ""
  , yellow    = ""
  , startup   = ""
  , follow    = ""
  , patterns  = ""
  }

newtype Xmodmap = Xmodmap
  { menu :: String
  } deriving (Data, Typeable)

defaultXmodmap :: Xmodmap
defaultXmodmap = Xmodmap
  { menu = ""
  }

newtype Xsession = Xsession
  { setxkbmap :: String
  } deriving (Data, Typeable)

defaultXsession :: Xsession
defaultXsession = Xsession
  { setxkbmap = ""
  }

data Urxvt = Urxvt
  { perllib, background_, browser :: String
  } deriving (Data, Typeable)

defaultUrxvt :: Urxvt
defaultUrxvt = Urxvt
  { perllib     = ""
  , background_ = ""
  , browser     = ""
  }
