--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.UrgencyHook (NoUrgencyHook(..), withUrgencyHook)
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
  xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { normalBorderColor  = "#1a1a1a"
    , focusedBorderColor = "#00bfff"
    , terminal           = "mlclient"
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
