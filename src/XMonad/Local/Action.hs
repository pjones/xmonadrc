{-# LANGUAGE FlexibleContexts #-}

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
  ( manageHook,
    handleEventHook,
  )
where

--------------------------------------------------------------------------------
import Control.Monad (when)
import Data.List
import qualified Data.Map as M
import Data.Monoid
import XMonad hiding (handleEventHook, manageHook)
import XMonad.Actions.TagWindows (addTag)
import XMonad.Hooks.InsertPosition (Focus (..), Position (..), insertPosition)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize (minimizeEventHook)
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------

-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
manageHook :: ManageHook
manageHook =
  composeOne
    [ -- Windows to ignore:
      isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR" -?> doIgnore,
      -- Start by tagging new windows:
      className =? "Chromium-browser" `addTagAndContinue` "browser",
      title =* "[irc]" `addTagAndContinue` "irc",
      -- Some application windows ask to be floating (I'm guessing) but
      -- it's stupid to float them.
      title =? "HandBrake" -?> (ask >>= doF . W.sink),
      -- Chrome debugging windows and application windows show up as
      -- pop-ups so we need to deal with that before floating pop-ups.
      ( className =? "Chromium-browser"
          <&&> stringProperty "WM_WINDOW_ROLE" =? "pop-up"
      )
        -?> normalTile,
      -- Force dialog windows and pop-ups to be floating.
      stringProperty "WM_WINDOW_ROLE" =? "pop-up" -?> doCenterFloat,
      stringProperty "WM_WINDOW_ROLE" =? gtkFile -?> forceCenterFloat,
      className =? "Gcr-prompter" -?> doCenterFloat,
      className =? "Pinentry" -?> doCenterFloat,
      transience, -- Move transient windows to their parent.
      isDialog -?> doCenterFloat,
      -- Certain windows shouldn't steal the master pane.
      className =? "konsole" -?> tileBelow,
      className =? "eterm" -?> tileBelow,
      className =? "Emacs" <&&> appName =? "popup" -?> tileBelowNoFocus,
      -- Tile all other windows using insertPosition.
      pure True -?> normalTile
    ]
  where
    (=*) :: Query String -> String -> Query Bool
    (=*) q s = isInfixOf s <$> q
    gtkFile = "GtkFileChooserDialog"
    normalTile = insertPosition Above Newer
    tileBelow = insertPosition Below Newer
    tileBelowNoFocus = insertPosition Below Older

--------------------------------------------------------------------------------

-- | If the given condition is 'True' then add the given tag name to
-- the window being mapped.  Always returns 'Nothing' to continue
-- processing other manage hooks.
addTagAndContinue :: Query Bool -> String -> MaybeManageHook
addTagAndContinue p tag = do
  x <- p
  when x (liftX . addTag tag =<< ask)
  return Nothing

--------------------------------------------------------------------------------

-- | Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.
forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h
    w, h, x, y :: Rational
    w = 1 / 2
    h = 1 / 2
    x = (1 - w) / 2
    y = (1 - h) / 2

--------------------------------------------------------------------------------
handleEventHook :: Event -> X All
handleEventHook =
  mconcat
    [ focusFollowsTiledOnly,
      minimizeEventHook
    ]

--------------------------------------------------------------------------------

-- | Enables 'focusFollowsMouse' for tiled windows only.  For this to
-- work you need to turn off 'focusFollowsMouse' in your configuration
-- and then add this function to your 'handleEventHook'.
focusFollowsTiledOnly :: Event -> X All
focusFollowsTiledOnly e@CrossingEvent {ev_window = w, ev_event_type = t}
  | isNormalEnter = whenX bothTiled (focus w) >> mempty
  where
    isNormalEnter = t == enterNotify && ev_mode e == notifyNormal
    bothTiled = notFloating w <&&> currentIsTiled
    currentIsTiled = currentWindow >>= maybe (return True) notFloating
    currentWindow = gets $ W.peek . windowset
    notFloating w' = gets $ not . M.member w' . W.floating . windowset
focusFollowsTiledOnly _ = mempty
