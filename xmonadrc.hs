--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import XMonad
import qualified XMonad.Local.Keys as Local

--------------------------------------------------------------------------------
main :: IO ()
main = xmonad $ defaultConfig
  { modMask            = mod3Mask
  , terminal           = "urxvtc"
  , borderWidth        = 3
  , normalBorderColor  = "#1a1a1a"
  , focusedBorderColor = "#00bfff"
  , focusFollowsMouse  = False
  , keys               = Local.keys
  }
