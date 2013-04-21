--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Utility functions dealing with XMonad events.
module XMonad.Local.Action
       ( manageHook
       , handleEventHook
       ) where

--------------------------------------------------------------------------------
import qualified Data.Map as M
import Data.Monoid
import XMonad hiding (manageHook, handleEventHook, tileWindow)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.FadeWindows (fadeWindowsEventHook)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  All elements given to
-- @composeAll@ are composed together so multiple matches can act on
-- the window set.  Therefore actions higher in the list to
-- @composeAll@ will have final say.
manageHook :: ManageHook
manageHook = manageDocks <> composeAll
  [ -- Force dialog windows and pop-ups to be floating.
    isDialog                                    --> doCenterFloat
  , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doCenterFloat

    -- Some application windows ask to be floating (I'm guessing) but
    -- it's stupid to float them.
  , title =? "HandBrake" --> tileWindow
  ]

--------------------------------------------------------------------------------
-- | Helper function to force a window to be tiled.
tileWindow :: ManageHook
tileWindow = ask >>= doF . W.sink

--------------------------------------------------------------------------------
handleEventHook :: Event -> X All
handleEventHook = mconcat [ fadeWindowsEventHook
                          , focusFollowsTiledOnly
                          , fullscreenEventHook
                          ]

--------------------------------------------------------------------------------
-- | Enables 'focusFollowsMouse' for tiled windows only.  For this to
-- work you need to turn off 'focusFollowsMouse' in your configuration
-- and then add this function to your 'handleEventHook'.
focusFollowsTiledOnly :: Event -> X All
focusFollowsTiledOnly e@(CrossingEvent {ev_window = w, ev_event_type = t})
  | isNormalEnter = whenX bothTiled (focus w) >> mempty
  where isNormalEnter   = t == enterNotify && ev_mode e == notifyNormal
        bothTiled       = notFloating w <&&> currentIsTiled
        currentIsTiled  = currentWindow >>= maybe (return True) notFloating
        currentWindow   = gets $ W.peek . windowset
        notFloating w'  = gets $ not . M.member w' . W.floating . windowset
focusFollowsTiledOnly _ = mempty
