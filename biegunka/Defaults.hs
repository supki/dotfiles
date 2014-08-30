{-# LANGUAGE DeriveDataTypeable #-}
module Defaults where

import Data.Data (Data, Typeable)

import Data.Default.Class (Default(..))


data Template = Template
  { xmobar   :: Xmobar
  , xmonad   :: Xmonad
  , xmodmap  :: Xmodmap
  , xsession :: Xsession
  , urxvt    :: Urxvt
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { xmobar   = def
    , xmonad   = def
    , xmodmap  = def
    , xsession = def
    , urxvt    = def
    }


data Xmobar = Xmobar
  { background, position :: String
  , battery              :: Maybe String
  } deriving (Data, Typeable)

instance Default Xmobar where
  def = Xmobar
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

instance Default Xmonad where
  def = Xmonad
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


data Xmodmap = Xmodmap
  { menu :: String
  } deriving (Data, Typeable)

instance Default Xmodmap where
  def = Xmodmap
    { menu = ""
    }


data Xsession = Xsession
  { setxkbmap :: String
  } deriving (Data, Typeable)

instance Default Xsession where
  def = Xsession
    { setxkbmap = ""
    }


data Urxvt = Urxvt
  { perllib, background_, browser :: String
  } deriving (Data, Typeable)

instance Default Urxvt where
  def = Urxvt
    { perllib     = ""
    , background_ = ""
    , browser     = ""
    }
