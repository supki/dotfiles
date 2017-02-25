{-# LANGUAGE DeriveDataTypeable #-}
module Defaults
  ( Template
  , Xmodmap(..)
  , Xmonad(..)
  , Xsession(..)
  , Urxvt(..)
  , template
  ) where

import Data.Data (Data, Typeable)


data Template = Template
  { xmonad   :: Xmonad
  , xmodmap  :: Xmodmap
  , xsession :: Xsession
  , urxvt    :: Urxvt
  } deriving (Data, Typeable)

type Mod a = a -> a

template :: Mod Xmonad -> Mod Xmodmap -> Mod Xsession -> Mod Urxvt -> Template
template g h j k = Template
  { xmonad   = g defaultXmonad
  , xmodmap  = h defaultXmodmap
  , xsession = j defaultXsession
  , urxvt    = k defaultUrxvt
  }

data Xmonad = Xmonad
  { terminus
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
  { perllib, background_ :: String
  } deriving (Data, Typeable)

defaultUrxvt :: Urxvt
defaultUrxvt = Urxvt
  { perllib     = ""
  , background_ = ""
  }
