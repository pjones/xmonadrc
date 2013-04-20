--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import XMonad
import qualified XMonad.Local.Action as Local
import qualified XMonad.Local.Keys   as Local
import qualified XMonad.Local.Layout as Local
import qualified XMonad.Local.Log    as Local
import qualified XMonad.Local.Workspaces as Workspaces
import XMonad.Util.Run (spawnPipe)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  xmobar <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { normalBorderColor  = "#1a1a1a"
    , focusedBorderColor = "#00bfff"
    , terminal           = "urxvtc"
    , layoutHook         = Local.layoutHook
    , manageHook         = Local.manageHook
    , handleEventHook    = Local.handleEventHook
    , workspaces         = Workspaces.names
    , modMask            = mod3Mask
    , keys               = Local.keys
    , borderWidth        = 3
    , logHook            = Local.logHook xmobar
    , focusFollowsMouse  = False
    }
