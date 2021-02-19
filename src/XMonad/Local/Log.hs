--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Utility functions for the XMonad @logHook@.
module XMonad.Local.Log (logHook) where

--------------------------------------------------------------------------------
import XMonad hiding (logHook)
import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Actions.SwapPromote (masterHistoryHook)

import XMonad.Hooks.FadeWindows
  ( FadeHook
  , fadeWindowsLogHook
  , transparency
  , opaque
  , isFloating
  )

import XMonad.Hooks.FadeInactive
  ( isUnfocusedOnCurrentWS
  )

--------------------------------------------------------------------------------
-- | XMonad @logHook@.
logHook :: X ()
logHook =
  historyHook <>
  masterHistoryHook <>
  fadeWindowsLogHook fadeHook

--------------------------------------------------------------------------------
-- | Control the opacity of windows.  The list is processed from the
-- bottom up.
fadeHook :: FadeHook
fadeHook =
  composeAll
    [ opaque,
      isUnfocusedOnCurrentWS --> transparency 0.1,
      isFloating --> opaque
    ]
