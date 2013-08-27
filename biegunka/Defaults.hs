{-# LANGUAGE DeriveDataTypeable #-}
module Defaults where

import Data.Data (Data, Typeable)

import Data.Default (Default(..))


data Template = Template
  { xmobar  :: Xmobar
  , xmonad  :: Xmonad
  , xmodmap :: Xmodmap
  , urxvt   :: Urxvt
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { xmobar  = def
    , xmonad  = def
    , xmodmap = def
    , urxvt   = def
    }


data Xmobar = Xmobar
  { background, position :: String
  , battery              :: Maybe String
  } deriving (Data, Typeable)

instance Default Xmobar where
  def = Xmobar
    { background = def
    , position   = def
    , battery    = Just "\"\""
    }


data Xmonad = Xmonad
  { terminal, ubuntu, terminus, white, grayDark, grayLight, black, orange, yellow, startup, follow :: String
  } deriving (Data, Typeable)

instance Default Xmonad where
  def = Xmonad
    { terminal  = def
    , ubuntu    = def
    , terminus  = def
    , white     = def
    , grayDark  = def
    , grayLight = def
    , black     = def
    , orange    = def
    , yellow    = def
    , startup   = def
    , follow    = def
    }


data Xmodmap = Xmodmap
  { menu :: String
  } deriving (Data, Typeable)

instance Default Xmodmap where
  def = Xmodmap
    { menu = def
    }


data Urxvt = Urxvt
  { perllib, background_, browser :: String
  } deriving (Data, Typeable)

instance Default Urxvt where
  def = Urxvt
    { perllib     = def
    , background_ = def
    , browser     = def
    }
